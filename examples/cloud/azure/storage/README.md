## links:
- https://registry.terraform.io/providers/hashicorp/azurerm/latest/docs/resources/storage_account
- https://registry.terraform.io/providers/hashicorp/azurerm/latest/docs/resources/storage_container
- https://registry.terraform.io/providers/hashicorp/azurerm/latest/docs/resources/storage_blob
- https://registry.terraform.io/providers/hashicorp/azurerm/latest/docs/data-sources/storage_blob
- https://registry.terraform.io/providers/hashicorp/azurerm/latest/docs/resources/storage_queue


## Run
```sh
# v run send_file_to_azure.v "<account_name>" "<account_key>" "<container_name>" "<blob_name>" "example.csv"
v run send_file_to_azure.v "devstoreaccount1" "Eby8vdM02xNOcqFlqUwJPLlmEtlCDXJ1OUzFT50uSRZ6IFsuFq2UVErCz4I6tq/K1SZFPTOtr/KBHBeksoGMGw==" "content" "blob_name" "example.csv"
```
