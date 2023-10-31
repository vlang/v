
#if !defined(PG_VERSION_NUM)
#error VERROR_MESSAGE PG_VERSION_NUM is not defined. Please install the development headers for PostgreSQL, they are usually in a package named libpq-dev
#endif

#if PG_VERSION_NUM < 100000
	#define CONNECTION_CHECK_WRITABLE 9
#endif

#if PG_VERSION_NUM < 100000
	#define CONNECTION_CONSUME 10
#endif

#if PG_VERSION_NUM < 120000
	#define CONNECTION_GSS_STARTUP 11
#endif
