## Installation

### Prerequisites
   - The IBM® Data De-Identification server requires Java™ 8 or higher and has been tested with OpenJDK™ 8 and the IBM® SDK, Java Technology Edition, Version 8.  To install Java on your system, we recommend downloading and installing OpenJDK 8 from `https://adoptopenjdk.net/`.
   - Apache® Maven™ is required to compile the server. It is downloadable as a compressed (zip) file from `https://maven.apache.org/download.cgi`. Only the binaries are required. Look for the link to either apache-maven-{version}-bin.zip or apache-maven-{version}-bin.tar.gz.

### Compilation
To compile the De-Identification server, clone the GIT repository and compile with Maven.

```
   export RELEASE=1.1.0   (change to the release you want to build)
   git clone --recurse-submodules https://github.com/Alvearie/de-identification
   cd de-identification/
   git checkout v${RELEASE}
   mvn clean package
```

 Note: `--recurse-submodules` is required because the repository contains a third-party encryption engine as a Git submodule.  For reference, the
 host repository for the submodule is `https://github.com/Alvearie/deid-java-fpe`, which is a modified fork of the third-party code available at `https://github.com/mysto/java-fpe`.

### Running the server

 - To start the server, use the following command: 

 ```
   java -jar de-identification-app/target/de-identification-app-${RELEASE}-exec.jar
 ```

 - After you start the server, verify that it is running properly. Invoke the health API:

 ```
   curl http://localhost:8080/api/v1/health
 ```

 In the response, you should see UP. The preceding command should produce output similar to:

 ```
   {"status":"UP"}
 ```

## Helm deployment
You can also deploy a release version of the De-Identification service to a Kubernetes cluster using Helm.  Container images for De-Identification releases are stored on Docker Hub.  For more information, see `de-identification/de-identification-app/chart/README.md`.

## WebApp security
By default, the de-identification Spring Boot server runs on insecure port 8080, which is not suitable for production deployment. Enable security that is suitable for your organization's needs. For basic security configuration, see [Spring Security Architecture](https://spring.io/guides/topicals/spring-security-architecture/).

## Next steps

- To learn more about the de-identification API, see [deid-rest-api.md](deid-rest-api.md). 
