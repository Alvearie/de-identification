FROM openjdk:8-jdk-alpine
ARG JAR_FILE=target/de-identification-app-1.0.1-SNAPSHOT-exec.jar
COPY ${JAR_FILE} app.jar
ENTRYPOINT ["java","-jar","/app.jar"]
# expose REST
EXPOSE 8080
