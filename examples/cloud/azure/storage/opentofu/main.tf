# Create Resource Group
resource "azurerm_resource_group" "rg_storage" {
  name     = var.resource_group_name
  location = var.location
}

# Create Storage Account
resource "azurerm_storage_account" "example" {
  name                     = var.storage_account_name # Storage account names must be globally unique and between 3-24 characters in length
  resource_group_name      = azurerm_resource_group.rg_storage.name
  location                 = azurerm_resource_group.rg_storage.location
  account_tier             = "Standard"
  account_replication_type = "LRS"
  min_tls_version          = "TLS1_2"

  tags = {
    environment = "Terraform Demo"
  }
}

# Create a Storage Container within the Storage Account
resource "azurerm_storage_container" "example" {
  name                  = var.storage_container_name
  storage_account_name  = azurerm_storage_account.example.name
  container_access_type = "private"
}

# Create a Storage Blob within the Storage Container
resource "azurerm_storage_blob" "example" {
  name                   = var.storage_blob_name
  storage_account_name   = azurerm_storage_account.example.name
  storage_container_name = azurerm_storage_container.example.name
  type                   = "Block"
  source                 = "example_blob.txt"
}

# Optional: Create a Storage Queue within the Storage Account
resource "azurerm_storage_queue" "example" {
  name                 = "examplequeue"
  storage_account_name = azurerm_storage_account.example.name
}
