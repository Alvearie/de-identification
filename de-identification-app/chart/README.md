# De-Identification Service Helm Chart

## Introduction

This [Helm](https://github.com/kubernetes/helm) chart installs an instance of the [Alvearie De-Identification](https://github.com/Alvearie/de-identification) service in a Kubernetes cluster.

## Pre-Requisites

- Kubernetes cluster 1.10+
- Helm 3.0.0+

## Installation

### Checkout the Code

Git clone this repository.

```bash
git clone https://github.com/Alvearie/de-identification.git
```

### Install the Chart

Install the helm chart with a desired release name, such as `deid`, and follow the resulting instructions:

```bash
git checkout <tag-of-release-you-wish-to-install>
cd de-identification/de-identification-app/chart
helm install deid .
```

### Using the Chart

Access your deid server at: `http://<external-ip>:8080/api/v1/deidentification` or port-forward to the deid service.

## Test your server

```bash
kubectl port-forward service/deid 8888:8080&
curl -k -v POST --header 'Content-Type: application/json' --header 'Accept: application/json' -d '{ "config":"{\"rules\":[{\"name\":\"HASH\",\"maskingProviders\":[{\"type\":\"HASH\"}]},{\"name\":\"PHONE\",\"maskingProviders\":[{\"type\":\"PHONE\"}]},{\"name\":\"NAME\",\"maskingProviders\":[{\"type\":\"NAME\"}]},{\"name\":\"MaskBirthDay\",\"maskingProviders\":[{\"type\":\"DATETIME\",\"generalizeYear\":true,\"yearMask\":false,\"monthMask\":false,\"dayMask\":false,\"hourMask\":false,\"minuteMask\":false,\"secondMask\":false}]}],\"json\":{\"schemaType\":\"FHIR\",\"messageTypeKey\":\"resourceType\",\"messageTypes\":[\"Patient\"],\"maskingRules\":[{\"jsonPath\":\"/fhir/Patient/name/given\",\"rule\":\"HASH\"},{\"jsonPath\":\"/fhir/Patient/name/family\",\"rule\":\"NAME\"},{\"jsonPath\":\"/fhir/Patient/telecom/value\",\"rule\":\"PHONE\"},{\"jsonPath\":\"/fhir/Patient/birthDate\",\"rule\":\"MaskBirthDay\"}]}}" , "data": [  "{\"resourceType\":\"Patient\",\"id\":\"example\",\"name\":[{\"use\":\"official\",\"family\":\"Chalmers\",\"given\":[\"Peter\",\"James\"]},{\"use\":\"usual\",\"given\":[\"Jim\"]}],\"telecom\":[{\"system\":\"phone\",\"value\":\"+1-3471234567\",\"use\":\"work\",\"rank\":1}],\"birthDate\":\"1974-12-25\"}"  ], "schemaType": "FHIR" }' 'http://localhost:8888/api/v1/deidentification'
```

## Uninstallation

To uninstall/delete the `deid` deployment:

```bash
helm delete deid
```

## Configuration

Each requirement is configured with the options provided by that Chart.
Please consult the relevant charts for their configuration options.

See `values.yaml`.

## Contributing

Feel free to contribute by making a [pull request](https://github.com/Alvearie/de-identification/pull/new/master).

Please review the [Contributing Guide](https://github.com/Alvearie/de-identification/blob/master/docs/contributing.md) for information on how to get started contributing to the project.

## License
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0) 
