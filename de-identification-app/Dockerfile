FROM openjdk:8-jdk-alpine
ARG RELEASE_VERSION
ARG JAR_FILE=target/de-identification-app-${RELEASE_VERSION}-exec.jar
COPY ${JAR_FILE} app.jar
ENTRYPOINT ["java","-jar","/app.jar"]
# expose REST
EXPOSE 8080
