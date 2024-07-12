
link: https://learn.microsoft.com/pt-br/azure/developer/terraform/create-avd-azure-files-storage

## TLDR;
```sh
terraform init -upgrade &&
terraform plan -out main.tfplan &&
terraform apply "main.tfplan"
```

## Inicializar o Terraform
```sh
terraform init -upgrade
```

## Criar um plano de execução do Terraform
```sh
terraform plan -out main.tfplan
```

## Aplicar um plano de execução do Terraform
```sh
terraform apply "main.tfplan"
```

## Limpar os recursos
```sh
terraform plan -destroy -out main.destroy.tfplan &&
terraform apply "main.destroy.tfplan"
```
