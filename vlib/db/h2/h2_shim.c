/*
 * h2_shim.c - JNI Invocation API bridge to the H2 JDBC driver.
 *
 * The JVM is created once per process. The path to jvm.dll comes from
 * JAVA_HOME at runtime and the JDBC jar from H2_JAR (or the jar_path
 * argument of h2_open). No absolute paths are baked into this file.
 */

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <jni.h>

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#else
#include <dlfcn.h>
#endif

#include "h2_shim.h"

static char g_last_error[2048];

static void h2_set_error(const char *msg) {
	if (msg == NULL) {
		msg = "unknown H2/JNI error";
	}
	snprintf(g_last_error, sizeof(g_last_error), "%s", msg);
}

const char *h2_last_error(void) {
	return g_last_error;
}

struct H2Conn {
	jobject conn;      /* global ref to java.sql.Connection */
	jclass conn_class; /* global ref to its class */
	jmethodID prepare;
	jmethodID prepare_keys;
	jmethodID create_statement;
	jmethodID close;
	long long last_id; /* auto-generated key captured from the last INSERT */
};

static JavaVM *g_jvm;
static void *g_jvm_lib;

typedef jint(JNICALL *CreateJavaVMFn)(JavaVM **pvm, void **penv, void *args);

/* Clear a pending Java exception and store its message in g_last_error. */
static void h2_clear_exception(JNIEnv *env) {
	jthrowable t;
	if (!(*env)->ExceptionCheck(env)) {
		return;
	}
	t = (*env)->ExceptionOccurred(env);
	(*env)->ExceptionClear(env);
	if (t != NULL) {
		jclass tc = (*env)->GetObjectClass(env, t);
		if (tc != NULL) {
			jmethodID gm = (*env)->GetMethodID(env, tc, "getMessage", "()Ljava/lang/String;");
			if (gm != NULL) {
				jstring msg = (jstring)(*env)->CallObjectMethod(env, t, gm);
				if (msg != NULL) {
					const char *s = (*env)->GetStringUTFChars(env, msg, NULL);
					if (s != NULL) {
						h2_set_error(s);
						(*env)->ReleaseStringUTFChars(env, msg, s);
					}
					(*env)->DeleteLocalRef(env, msg);
				}
			}
			(*env)->DeleteLocalRef(env, tc);
		}
		(*env)->DeleteLocalRef(env, t);
	}
}

static jmethodID h2_mid(JNIEnv *env, jclass cls, const char *name, const char *sig) {
	jmethodID m;
	if (cls == NULL) {
		return NULL;
	}
	m = (*env)->GetMethodID(env, cls, name, sig);
	if (m == NULL) {
		h2_clear_exception(env);
	}
	return m;
}

static char *h2_jstring_copy(JNIEnv *env, jstring s) {
	const char *raw;
	char *out;
	if (s == NULL) {
		return NULL;
	}
	raw = (*env)->GetStringUTFChars(env, s, NULL);
	if (raw == NULL) {
		return NULL;
	}
	out = (char *)malloc(strlen(raw) + 1);
	if (out != NULL) {
		strcpy(out, raw);
	}
	(*env)->ReleaseStringUTFChars(env, s, raw);
	return out;
}

/* Create the JVM on first use, otherwise attach the calling thread. */
static JNIEnv *h2_get_env(const char *jar_path) {
	JNIEnv *env = NULL;
	if (g_jvm == NULL) {
		CreateJavaVMFn create_fn = NULL;
#ifdef _WIN32
		const char *home = getenv("JAVA_HOME");
		char dll_path[4096];
		if (home != NULL && *home != '\0') {
			snprintf(dll_path, sizeof(dll_path), "%s\\bin\\server\\jvm.dll", home);
		} else {
			snprintf(dll_path, sizeof(dll_path), "jvm.dll");
		}
		g_jvm_lib = (void *)LoadLibraryA(dll_path);
		if (g_jvm_lib == NULL) {
			snprintf(g_last_error, sizeof(g_last_error), "h2: cannot load %s (set JAVA_HOME)",
				dll_path);
			return NULL;
		}
		create_fn = (CreateJavaVMFn)GetProcAddress((HMODULE)g_jvm_lib, "JNI_CreateJavaVM");
#else
		g_jvm_lib = dlopen("libjvm.so", RTLD_NOW | RTLD_GLOBAL);
		if (g_jvm_lib != NULL) {
			create_fn = (CreateJavaVMFn)dlsym(g_jvm_lib, "JNI_CreateJavaVM");
		}
#endif
		if (create_fn == NULL) {
			h2_set_error("h2: JNI_CreateJavaVM entry point not found");
			return NULL;
		}
		{
			char cp[8192];
			const char *jar = (jar_path != NULL && *jar_path != '\0') ? jar_path : getenv("H2_JAR");
			JavaVMOption opts[1];
			JavaVMInitArgs args;
			jint rc;
			if (jar != NULL && *jar != '\0') {
				snprintf(cp, sizeof(cp), "-Djava.class.path=%s", jar);
			} else {
				snprintf(cp, sizeof(cp), "-Djava.class.path=.");
			}
			opts[0].optionString = cp;
			opts[0].extraInfo = NULL;
			args.version = JNI_VERSION_1_8;
			args.nOptions = 1;
			args.options = opts;
			args.ignoreUnrecognized = JNI_FALSE;
			rc = create_fn(&g_jvm, (void **)&env, &args);
			if (rc != JNI_OK || g_jvm == NULL || env == NULL) {
				snprintf(g_last_error, sizeof(g_last_error), "h2: JNI_CreateJavaVM failed (%d)",
					(int)rc);
				return NULL;
			}
		}
	} else {
		jint rc = (*g_jvm)->GetEnv(g_jvm, (void **)&env, JNI_VERSION_1_8);
		if (rc == JNI_EDETACHED) {
			rc = (*g_jvm)->AttachCurrentThread(g_jvm, (void **)&env, NULL);
		}
		if (rc != JNI_OK || env == NULL) {
			h2_set_error("h2: cannot attach the calling thread to the JVM");
			return NULL;
		}
	}
	return env;
}

H2Conn *h2_open(const char *url, const char *jar_path) {
	JNIEnv *env;
	jclass dm, dcls;
	jmethodID getconn;
	jstring jurl;
	jobject jconn;
	jclass conn_cls;
	H2Conn *c;

	g_last_error[0] = '\0';
	env = h2_get_env(jar_path);
	if (env == NULL) {
		return NULL;
	}
	dm = (*env)->FindClass(env, "java/sql/DriverManager");
	if (dm == NULL) {
		h2_clear_exception(env);
		h2_set_error("h2: java.sql.DriverManager not found");
		return NULL;
	}
	/* Explicitly register the driver; JDBC 4 auto-discovery is not assumed. */
	dcls = (*env)->FindClass(env, "org/h2/Driver");
	if (dcls != NULL) {
		jmethodID ctor = (*env)->GetMethodID(env, dcls, "<init>", "()V");
		if (ctor != NULL) {
			jobject drv = (*env)->NewObject(env, dcls, ctor);
			if (drv != NULL) {
				jmethodID reg =
					(*env)->GetStaticMethodID(env, dm, "registerDriver", "(Ljava/sql/Driver;)V");
				if (reg != NULL) {
					(*env)->CallStaticVoidMethod(env, dm, reg, drv);
				}
				(*env)->DeleteLocalRef(env, drv);
			}
		}
		(*env)->DeleteLocalRef(env, dcls);
	} else {
		(*env)->ExceptionClear(env);
	}
	getconn = (*env)->GetStaticMethodID(env, dm, "getConnection",
		"(Ljava/lang/String;)Ljava/sql/Connection;");
	if (getconn == NULL) {
		h2_clear_exception(env);
		(*env)->DeleteLocalRef(env, dm);
		return NULL;
	}
	jurl = (*env)->NewStringUTF(env, url);
	jconn = (*env)->CallStaticObjectMethod(env, dm, getconn, jurl);
	(*env)->DeleteLocalRef(env, jurl);
	(*env)->DeleteLocalRef(env, dm);
	if (jconn == NULL || (*env)->ExceptionCheck(env)) {
		h2_clear_exception(env);
		if (g_last_error[0] == '\0') {
			h2_set_error("h2: DriverManager.getConnection returned null");
		}
		return NULL;
	}
	c = (H2Conn *)calloc(1, sizeof(H2Conn));
	conn_cls = (*env)->GetObjectClass(env, jconn);
	c->conn = (*env)->NewGlobalRef(env, jconn);
	c->conn_class = (jclass)(*env)->NewGlobalRef(env, conn_cls);
	c->prepare = h2_mid(env, conn_cls, "prepareStatement",
		"(Ljava/lang/String;)Ljava/sql/PreparedStatement;");
	c->prepare_keys = h2_mid(env, conn_cls, "prepareStatement",
		"(Ljava/lang/String;I)Ljava/sql/PreparedStatement;");
	c->create_statement = h2_mid(env, conn_cls, "createStatement", "()Ljava/sql/Statement;");
	c->close = h2_mid(env, conn_cls, "close", "()V");
	(*env)->DeleteLocalRef(env, conn_cls);
	(*env)->DeleteLocalRef(env, jconn);
	if (c->prepare == NULL || c->close == NULL || c->create_statement == NULL) {
		free(c);
		return NULL;
	}
	return c;
}

void h2_close(H2Conn *conn) {
	JNIEnv *env;
	if (conn == NULL) {
		return;
	}
	env = h2_get_env(NULL);
	if (env != NULL) {
		if (conn->close != NULL) {
			(*env)->CallVoidMethod(env, conn->conn, conn->close);
		}
		h2_clear_exception(env);
		(*env)->DeleteGlobalRef(env, conn->conn);
		(*env)->DeleteGlobalRef(env, conn->conn_class);
	}
	free(conn);
}

/* Bind the parameters of a PreparedStatement.
 * Values are passed as strings by the V layer. Pure integers use setInt so
 * that LIMIT/OFFSET placeholders are accepted by H2; everything else uses
 * setString, and nulls use setNull. */
static int h2_bind(JNIEnv *env, jobject pstmt, jclass ps_cls, const char **params,
	const long long *nulls, int nparams) {
	jmethodID set_string = h2_mid(env, ps_cls, "setString", "(ILjava/lang/String;)V");
	jmethodID set_int = h2_mid(env, ps_cls, "setInt", "(II)V");
	jmethodID set_null = h2_mid(env, ps_cls, "setNull", "(II)V");
	int i;
	if (set_string == NULL || set_null == NULL) {
		return 0;
	}
	for (i = 0; i < nparams; ++i) {
		const char *s = params[i];
		if (nulls[i]) {
			(*env)->CallVoidMethod(env, pstmt, set_null, i + 1, 0 /* java.sql.Types.NULL */);
			continue;
		}
		if (s != NULL && set_int != NULL && *s != '\0') {
			char *end = NULL;
			long v = strtol(s, &end, 10);
			if (end != NULL && *end == '\0' && v >= -2147483647L && v <= 2147483647L) {
				(*env)->CallVoidMethod(env, pstmt, set_int, i + 1, (jint)v);
				continue;
			}
		}
		{
			jstring js = (*env)->NewStringUTF(env, s != NULL ? s : "");
			if (js == NULL) {
				return 0;
			}
			(*env)->CallVoidMethod(env, pstmt, set_string, i + 1, js);
			(*env)->DeleteLocalRef(env, js);
		}
		if ((*env)->ExceptionCheck(env)) {
			h2_clear_exception(env);
			return 0;
		}
	}
	return 1;
}

static void h2_read_result_set(JNIEnv *env, jobject rs, H2Result *res) {
	jclass rs_cls = (*env)->GetObjectClass(env, rs);
	jmethodID get_md = h2_mid(env, rs_cls, "getMetaData", "()Ljava/sql/ResultSetMetaData;");
	jmethodID next = h2_mid(env, rs_cls, "next", "()Z");
	jmethodID get_string = h2_mid(env, rs_cls, "getString", "(I)Ljava/lang/String;");
	jmethodID was_null = h2_mid(env, rs_cls, "wasNull", "()Z");
	jobject md = NULL;
	jclass md_cls = NULL;
	jmethodID get_cc, get_cn;
	int i, cap;

	if (get_md == NULL || next == NULL || get_string == NULL || was_null == NULL) {
		goto done;
	}
	md = (*env)->CallObjectMethod(env, rs, get_md);
	if (md == NULL) {
		h2_clear_exception(env);
		goto done;
	}
	md_cls = (*env)->GetObjectClass(env, md);
	get_cc = h2_mid(env, md_cls, "getColumnCount", "()I");
	get_cn = h2_mid(env, md_cls, "getColumnName", "(I)Ljava/lang/String;");
	if (get_cc == NULL || get_cn == NULL) {
		goto done;
	}
	res->ncols = (int)(*env)->CallIntMethod(env, md, get_cc);
	if (res->ncols <= 0) {
		goto done;
	}
	res->col_names = (char **)calloc((size_t)res->ncols, sizeof(char *));
	for (i = 0; i < res->ncols; ++i) {
		jstring jn = (jstring)(*env)->CallObjectMethod(env, md, get_cn, i + 1);
		res->col_names[i] = h2_jstring_copy(env, jn);
		if (jn != NULL) {
			(*env)->DeleteLocalRef(env, jn);
		}
	}
	cap = 16;
	res->vals = (char **)calloc((size_t)cap * (size_t)res->ncols, sizeof(char *));
	while ((*env)->CallBooleanMethod(env, rs, next) == JNI_TRUE) {
		int j;
		if (res->nrows >= cap) {
			char **grown;
			cap *= 2;
			grown = (char **)realloc(res->vals,
				(size_t)cap * (size_t)res->ncols * sizeof(char *));
			if (grown == NULL) {
				break;
			}
			res->vals = grown;
			memset(res->vals + (size_t)res->nrows * (size_t)res->ncols, 0,
				(size_t)(cap - res->nrows) * (size_t)res->ncols * sizeof(char *));
		}
		for (j = 0; j < res->ncols; ++j) {
			jstring jv = (jstring)(*env)->CallObjectMethod(env, rs, get_string, j + 1);
			jboolean isnull = (*env)->CallBooleanMethod(env, rs, was_null);
			char **slot = res->vals + (size_t)res->nrows * (size_t)res->ncols + j;
			if (isnull == JNI_TRUE || jv == NULL) {
				*slot = NULL;
			} else {
				*slot = h2_jstring_copy(env, jv);
			}
			if (jv != NULL) {
				(*env)->DeleteLocalRef(env, jv);
			}
		}
		res->nrows++;
	}
	if ((*env)->ExceptionCheck(env)) {
		h2_clear_exception(env);
	}
done:
	/* JNI local refs live until the thread detaches; this bridge never detaches,
	 * so release them explicitly to avoid accumulating one set per query. */
	if (md_cls != NULL) {
		(*env)->DeleteLocalRef(env, md_cls);
	}
	if (md != NULL) {
		(*env)->DeleteLocalRef(env, md);
	}
	(*env)->DeleteLocalRef(env, rs_cls);
}

H2Result *h2_exec(H2Conn *conn, const char *sql, const char **params, const long long *nulls,
	int nparams) {
	JNIEnv *env;
	H2Result *res;
	jstring jsql;
	jobject pstmt;
	jclass ps_cls;
	jmethodID execute, get_rs;
	jboolean has_rs;

	g_last_error[0] = '\0';
	if (conn == NULL || sql == NULL) {
		h2_set_error("h2: invalid connection or query");
		return NULL;
	}
	env = h2_get_env(NULL);
	if (env == NULL) {
		return NULL;
	}
	jsql = (*env)->NewStringUTF(env, sql);
	{
		const char *p = sql;
		int is_insert;
		while (*p != '\0' && isspace((unsigned char)*p)) {
			p++;
		}
		is_insert = (strncmp(p, "INSERT", 6) == 0 || strncmp(p, "insert", 6) == 0);
		if (is_insert && conn->prepare_keys != NULL) {
			/* Statement.RETURN_GENERATED_KEYS == 1 */
			pstmt = (*env)->CallObjectMethod(env, conn->conn, conn->prepare_keys, jsql, 1);
		} else {
			pstmt = (*env)->CallObjectMethod(env, conn->conn, conn->prepare, jsql);
		}
	}
	(*env)->DeleteLocalRef(env, jsql);
	if (pstmt == NULL || (*env)->ExceptionCheck(env)) {
		h2_clear_exception(env);
		return NULL;
	}
	ps_cls = (*env)->GetObjectClass(env, pstmt);
	if (ps_cls == NULL || !h2_bind(env, pstmt, ps_cls, params, nulls, nparams)) {
		if (ps_cls != NULL) {
			(*env)->DeleteLocalRef(env, ps_cls);
		}
		(*env)->DeleteLocalRef(env, pstmt);
		return NULL;
	}
	execute = h2_mid(env, ps_cls, "execute", "()Z");
	get_rs = h2_mid(env, ps_cls, "getResultSet", "()Ljava/sql/ResultSet;");
	if (execute == NULL || get_rs == NULL) {
		(*env)->DeleteLocalRef(env, ps_cls);
		(*env)->DeleteLocalRef(env, pstmt);
		return NULL;
	}
	has_rs = (*env)->CallBooleanMethod(env, pstmt, execute);
	if ((*env)->ExceptionCheck(env)) {
		h2_clear_exception(env);
		(*env)->DeleteLocalRef(env, ps_cls);
		(*env)->DeleteLocalRef(env, pstmt);
		return NULL;
	}
	res = (H2Result *)calloc(1, sizeof(H2Result));
	if (has_rs == JNI_TRUE) {
		jobject rs = (*env)->CallObjectMethod(env, pstmt, get_rs);
		if (rs != NULL) {
			h2_read_result_set(env, rs, res);
			(*env)->DeleteLocalRef(env, rs);
		} else {
			h2_clear_exception(env);
		}
	} else {
		jmethodID get_count = h2_mid(env, ps_cls, "getUpdateCount", "()I");
		if (get_count != NULL) {
			(*env)->CallIntMethod(env, pstmt, get_count);
			if ((*env)->ExceptionCheck(env)) {
				h2_clear_exception(env);
			}
		}
		{
			const char *p = sql;
			while (*p != '\0' && isspace((unsigned char)*p)) {
				p++;
			}
			if (strncmp(p, "INSERT", 6) == 0 || strncmp(p, "insert", 6) == 0) {
				jmethodID get_keys =
					h2_mid(env, ps_cls, "getGeneratedKeys", "()Ljava/sql/ResultSet;");
				if (get_keys != NULL) {
					jobject keys = (*env)->CallObjectMethod(env, pstmt, get_keys);
					if (keys != NULL && !(*env)->ExceptionCheck(env)) {
						jclass kcls = (*env)->GetObjectClass(env, keys);
						jmethodID knext = h2_mid(env, kcls, "next", "()Z");
						jmethodID kstr = h2_mid(env, kcls, "getString", "(I)Ljava/lang/String;");
						if (knext != NULL && kstr != NULL
							&& (*env)->CallBooleanMethod(env, keys, knext) == JNI_TRUE) {
							jstring jv = (jstring)(*env)->CallObjectMethod(env, keys, kstr, 1);
							if (jv != NULL) {
								const char *s = (*env)->GetStringUTFChars(env, jv, NULL);
								if (s != NULL) {
									conn->last_id = strtoll(s, NULL, 10);
									(*env)->ReleaseStringUTFChars(env, jv, s);
								}
								(*env)->DeleteLocalRef(env, jv);
							}
						}
						(*env)->DeleteLocalRef(env, kcls);
						(*env)->DeleteLocalRef(env, keys);
					} else {
						h2_clear_exception(env);
					}
				}
			}
		}
	}
	(*env)->DeleteLocalRef(env, ps_cls);
	(*env)->DeleteLocalRef(env, pstmt);
	return res;
}

void h2_result_free(H2Result *res) {
	int i;
	if (res == NULL) {
		return;
	}
	for (i = 0; i < res->ncols; ++i) {
		free(res->col_names[i]);
	}
	for (i = 0; i < res->nrows * res->ncols; ++i) {
		free(res->vals[i]);
	}
	free(res->col_names);
	free(res->vals);
	free(res);
}

long long h2_last_id(H2Conn *conn) {
	/* H2 2.x no longer provides IDENTITY()/LAST_INSERT_ID(); the generated
	 * keys are captured from each INSERT in h2_exec instead. */
	return conn == NULL ? 0 : conn->last_id;
}
