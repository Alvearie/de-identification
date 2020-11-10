/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.app.endpoint;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.data.mongo.MongoDataAutoConfiguration;
import org.springframework.boot.autoconfigure.mongo.MongoAutoConfiguration;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Primary;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.scheduling.annotation.EnableScheduling;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ibm.whc.deid.ObjectMapperFactory;
import springfox.documentation.swagger2.annotations.EnableSwagger2;

/*
 * Main SpringBoot application class
 */
@SpringBootApplication(exclude = {MongoAutoConfiguration.class, MongoDataAutoConfiguration.class})
@EnableSwagger2
@EnableScheduling
@EnableAsync
@ComponentScan(basePackages = {"com.ibm.whc.deid.app.endpoint", "com.ibm.whc.deid.endpoint"})
public class Application {

  public static void main(String[] args) {
    // The args are not used
    SpringApplication.run(Application.class);
  }

  @Bean
  @Primary
  public ObjectMapper objectMapper() {
    return ObjectMapperFactory.getObjectMapper();
  }

}
