## Installation

 - Prerequisite: 
   - The IBM Data De-Identification server requires Java 8 or higher and has been tested with OpenJDK 8 and the IBM SDK, Java Technology Edition, Version 8.  To install Java on your system, we recommend downloading and installaing OpenJDK 8 from `https://adoptopenjdk.net/.`
   - Maven is required to compile the server.  It is downloadable as a zip file from `https://maven.apache.org/download.cgi`.  Only the binaries are required, please look for the link to apache-maven-{version}-bin.zip or apache-maven-{version}-bin.tar.gz.

 - Compiplation:  To compile the De-Identification server, clone the GIT repository and compile with maven.

```
   git clone https://github.com/Alvearie/de-identification
   cd de-identification/
   mvn clean package
```

 - To Start the server, use the following command:

```
   java -jar target/de-identification-app-0.0.1.master-SNAPSHOT-exec.jar
```

 - After you start the server, you can verify that it is running properly by invoking the health API:

 ```
   curl http://localhost:8080/api/v1/health
 ```

 One should see UP in the response. The preceding command should produce output similar to the following:

 ```
   {"status":"UP"}
 ``` 

## WebApp security

By default, the de-identification Spring Boot server runs on insecure port 8080 which is not suitable for production deployment.  Please enable security that fits your organization's need. For basic security configuration, please refer to : [Spring Security Architecture](https://spring.io/guides/topicals/spring-security-architecture/) 


## Next steps

- To learn more about the de-identification API, please refer to : [deid-rest-api.md](deid-rest-api.md)
