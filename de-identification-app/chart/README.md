# De-Identification Service Helm Chart

## Introduction

This [Helm](https://github.com/kubernetes/helm) chart installs an instance of the [Alvearie De-Identification](https://github.com/Alvearie/de-identification) service in a Kubernetes cluster.

## Pre-Requisites

- Kubernetes cluster 1.10+
- Helm 3.0.0+

## Installation

### Checkout the Code

Git clone this repository and `cd` into this directory.

```bash
git clone https://github.com/Alvearie/health-patterns.git
cd de-identificaiton-app/chart
```

### Install the Chart

Install the helm chart with a desired release name, such as `deid` and follow the resulting instructions:

```bash
helm install deid .
```

### Using the Chart

Access your FHIR server at: `http://<external-ip>:8080/api/v1/health`

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
