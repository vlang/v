https://learn.microsoft.com/pt-br/rest/api/storageservices/authorize-with-shared-key

# Blob, Fila e Serviços de Arquivos (autorização de chave compartilhada)

```
StringToSign = VERB + "\n" +
               Content-Encoding + "\n" +
               Content-Language + "\n" +
               Content-Length + "\n" +
               Content-MD5 + "\n" +
               Content-Type + "\n" +
               Date + "\n" +
               If-Modified-Since + "\n" +
               If-Match + "\n" +
               If-None-Match + "\n" +
               If-Unmodified-Since + "\n" +
               Range + "\n" +
               CanonicalizedHeaders +
               CanonicalizedResource;
```

e.g.

```
GET\n\n\n\n\n\n\n\n\n\n\n\nx-ms-date:Fri, 26 Jun 2015 23:39:12 GMT\nx-ms-version:2015-02-21\n/myaccount/mycontainer\ncomp:metadata\nrestype:container\ntimeout:20
```

or

```
GET\n /*HTTP Verb*/
\n    /*Content-Encoding*/
\n    /*Content-Language*/
\n    /*Content-Length (empty string when zero)*/
\n    /*Content-MD5*/
\n    /*Content-Type*/
\n    /*Date*/
\n    /*If-Modified-Since */
\n    /*If-Match*/
\n    /*If-None-Match*/
\n    /*If-Unmodified-Since*/
\n    /*Range*/
x-ms-date:Fri, 26 Jun 2015 23:39:12 GMT\nx-ms-version:2015-02-21\n    /*CanonicalizedHeaders*/
/myaccount /mycontainer\ncomp:metadata\nrestype:container\ntimeout:20    /*CanonicalizedResource*/
```

```
# To authorize a request, you must sign the request with the key for the account that is making the request and pass that signature as part of the request.
Authorization="[SharedKey|SharedKeyLite] <AccountName>:<Signature>"
```

In the context of Azure Blob Storage, canonicalized headers are used to create a standardized and predictable string representation of HTTP headers for signing requests. This is an important part of the authentication process, especially when using Shared Key authentication.

Here is a list of the canonicalized headers required for signing Azure Blob Storage requests:

### Required Canonicalized Headers

1. **x-ms-date**
2. **x-ms-version**

These headers must be included in the string-to-sign for authentication.

### Optional Canonicalized Headers

These headers are prefixed with `x-ms-` and are optional but must be included in the canonicalized headers if they are present in the request:

1. **x-ms-meta-name:value**: Metadata headers, where `name` is a custom metadata name, and `value` is the metadata value.
2. **x-ms-blob-content-type**: Specifies the MIME content type of the blob.
3. **x-ms-blob-content-encoding**: Specifies which content encodings have been applied to the blob.
4. **x-ms-blob-content-language**: Specifies the natural languages used by this resource.
5. **x-ms-blob-content-length**: Specifies the size of the blob.
6. **x-ms-blob-content-md5**: An MD5 hash of the blob content to ensure data integrity.
7. **x-ms-blob-cache-control**: Specifies blob cache control settings.
8. **x-ms-blob-content-disposition**: Specifies the content disposition of the blob.
9. **x-ms-lease-id**: Lease ID, if the blob has an active lease.
10. **x-ms-range**: Byte range for downloading a section of the blob.
11. **x-ms-delete-snapshots**: Required if the blob has associated snapshots.
12. **x-ms-blob-type**: Specifies the blob type (e.g., BlockBlob, PageBlob).
13. **x-ms-copy-source**: The URL of the source blob to copy from.
14. **x-ms-copy-source-range**: The range of bytes to copy from the source blob.
15. **x-ms-copy-id**: The ID of the copy operation.
16. **x-ms-copy-status**: The status of the copy operation.

### Creating the Canonicalized Headers String

To create the canonicalized headers string, follow these steps:

1. **Collect all `x-ms-` headers**: Include all headers that start with `x-ms-`.
2. **Convert to lowercase**: Convert each header name to lowercase.
3. **Sort headers**: Sort the headers lexicographically by their names.
4. **Trim whitespace**: Ensure that no leading or trailing whitespace exists in the header values.
5. **Format headers**: Format each header as `header-name:value` where `header-name` is the header in lowercase and `value` is the trimmed value.
6. **Join headers**: Join all formatted headers using newline characters (`\n`).

### Example

Assume you have the following headers in your request:

- `x-ms-date: Fri, 19 Jun 2020 10:23:42 GMT`
- `x-ms-version: 2020-04-08`
- `x-ms-meta-name: value`
- `x-ms-blob-type: BlockBlob`

The canonicalized headers string would be:

```
x-ms-blob-type:blockblob
x-ms-date:fri, 19 jun 2020 10:23:42 gmt
x-ms-meta-name:value
x-ms-version:2020-04-08
```

### Sample String-to-Sign

To demonstrate, let's use a string-to-sign that includes the canonicalized headers for a hypothetical PUT request:

#### Example Request:

```
PUT https://mystorageaccount.blob.core.windows.net/mycontainer/myblob
x-ms-date: Fri, 19 Jun 2020 10:23:42 GMT
x-ms-version: 2020-04-08
x-ms-meta-name: value
x-ms-blob-type: BlockBlob
```

#### Example String-to-Sign:

```
PUT

text/plain
12345
Fri, 19 Jun 2020 10:23:42 GMT

x-ms-blob-type:blockblob
x-ms-date:fri, 19 jun 2020 10:23:42 gmt
x-ms-meta-name:value
x-ms-version:2020-04-08
/mystorageaccount/mycontainer/myblob
```

Canonicalized Resource is a string that represents the resource being accessed in a standardized format. It is used as part of the string-to-sign for authenticating requests to Azure Blob Storage using Shared Key authorization. The Canonicalized Resource string ensures that the request is correctly authenticated by including the resource path and any relevant query parameters.

Here are the key components and steps to construct the Canonicalized Resource string for Azure Blob Storage:

### Components of the Canonicalized Resource String

1. **Account Name**: The name of the storage account.
2. **Resource Path**: The path to the resource, which includes the container name and blob name.
3. **Query Parameters**: Canonicalized representation of any query parameters.

### Basic Structure

The basic structure for a Canonicalized Resource string is:

```
/{account-name}/{resource-path}
```

### Steps to Construct the Canonicalized Resource String

1. **Start with a slash (`/`) followed by the storage account name**:

   ```
   /{account-name}
   ```

2. **Append the resource path (container name and blob name)**:

   ```
   /{account-name}/{container-name}/{blob-name}
   ```

3. **Include any query parameters in a specific order**: If the request URL contains any query parameters, they must be included in the Canonicalized Resource string in a specific way. The query parameters should be sorted by parameter name in lexicographical order.

### Handling Query Parameters

For certain operations, such as listing blobs or containers, query parameters need to be included in the Canonicalized Resource string. Here are the query parameters that need to be included:

- **comp**
- **restype**
- **timeout**

If multiple query parameters are present, they should be sorted and included in the following format:

```
{parameter-name}:{parameter-value}\n
```

### Example Construction

Consider an example where you want to access a blob within a container:

#### Example Request:

```
GET https://mystorageaccount.blob.core.windows.net/mycontainer/myblob?comp=metadata&timeout=30
```

1. **Account Name**:

   ```
   mystorageaccount
   ```

2. **Resource Path**:

   ```
   /mycontainer/myblob
   ```

3. **Sorted Query Parameters**:
   ```
   comp:metadata
   timeout:30
   ```

#### Canonicalized Resource String:

```
/mystorageaccount/mycontainer/myblob
comp:metadata
timeout:30
```

### Detailed Example

#### Example Request:

```
GET https://mystorageaccount.blob.core.windows.net/mycontainer?restype=container&comp=list&timeout=30
```

1. **Account Name**:

   ```
   mystorageaccount
   ```

2. **Resource Path**:

   ```
   /mycontainer
   ```

3. **Sorted Query Parameters**:
   ```
   comp:list
   restype:container
   timeout:30
   ```

#### Canonicalized Resource String:

```
/mystorageaccount/mycontainer
comp:list
restype:container
timeout:30
```

### Sample String-to-Sign

To demonstrate how the Canonicalized Resource string fits into the string-to-sign, let’s use the previous example:

#### Example Request:

```
GET https://mystorageaccount.blob.core.windows.net/mycontainer/myblob?comp=metadata&timeout=30
x-ms-date: Fri, 19 Jun 2020 10:23:42 GMT
x-ms-version: 2020-04-08
```

#### String-to-Sign:

```
GET

application/octet-stream
12345
Fri, 19 Jun 2020 10:23:42 GMT

x-ms-date:fri, 19 jun 2020 10:23:42 gmt
x-ms-version:2020-04-08
/mystorageaccount/mycontainer/myblob
comp:metadata
timeout:30
```

### Conclusion

The Canonicalized Resource string is a critical component for signing requests to Azure Blob Storage, ensuring that requests are authenticated properly. By correctly constructing this string and including all necessary components in the specified order, you can securely access Azure Blob Storage resources.
