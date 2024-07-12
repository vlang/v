variable "resource_group_name" {
  description = "The name of the resource group in which to create the storage account."
  type        = string
  default     = "example-resources2"
}

variable "location" {
  description = "The location in which to create the resources."
  type        = string
  default     = "eastus"
}

variable "storage_account_name" {
  description = "The name of the storage account."
  type        = string
  default     = "devstoreaccount1"  # Ensure this name is unique, can only consist of lowercase letters and numbers, and must be between 3 and 24 characters long
}

variable "storage_container_name" {
  description = "The name of the storage container."
  type        = string
  default     = "content"
}

variable "storage_blob_name" {
  description = "The name of the storage blob."
  type        = string
  default     = "example_blob.txt"
}
