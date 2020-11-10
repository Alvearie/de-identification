## Installation

 - Prerequisite: 
   - The IBM® Data De-Identification server requires Java™ 8 or higher and has been tested with OpenJDK™ 8 and the IBM® SDK, Java Technology Edition, Version 8.  To install Java on your system, we recommend downloading and installing OpenJDK 8 from `https://adoptopenjdk.net/`.
   - Apache® Maven™ is required to compile the server. It is downloadable as a compressed (zip) file from `https://maven.apache.org/download.cgi`. Only the binaries are required. Look for the link to either apache-maven-{version}-bin.zip or apache-maven-{version}-bin.tar.gz.

 - Compilation: To compile the De-Identification server, clone the GIT repository and compile with Maven.

```
   git clone https://github.com/Alvearie/de-identification
   cd de-identification/
   mvn clean package
```

 - To start the server, use the following command: 

```
   java -jar target/de-identification-app-0.0.1.master-SNAPSHOT-exec.jar
```

 - After you start the server, verify that it is running properly. Invoke the health API:

 ```
   curl http://localhost:8080/api/v1/health
 ```

 In the response, you should see UP. The preceding command should produce output similar to:

 ```
   {"status":"UP"}
 ``` 

## WebApp security

By default, the de-identification Spring Boot server runs on insecure port 8080, which is not suitable for production deployment. Enable security that is suitable for your organization's needs. For basic security configuration, see [Spring Security Architecture](https://spring.io/guides/topicals/spring-security-architecture/)


## Next steps

- To learn more about the de-identification API, see [deid-rest-api.md](deid-rest-api.md). 