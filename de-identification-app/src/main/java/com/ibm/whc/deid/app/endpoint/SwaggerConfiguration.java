/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.app.endpoint;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import com.google.common.base.Predicates;

import springfox.documentation.builders.ApiInfoBuilder;
import springfox.documentation.builders.PathSelectors;
import springfox.documentation.builders.RequestHandlerSelectors;
import springfox.documentation.service.ApiInfo;
import springfox.documentation.spi.DocumentationType;
import springfox.documentation.spring.web.plugins.Docket;

/**
 * Use @Configuration so that Spring creates a Spring bean in the application context
 *
 */
@Configuration
public class SwaggerConfiguration {

  ApiInfo apiInfo() {

    return new ApiInfoBuilder().title("IBM Data De-Identification")
    		   .description(
    		            "The Data De-Identification service provides a wide range of de-identification capabilities designed to support GDPR, HIPAA, CCPA and other privacy frameworks allowing customers to meet their regulatory and privacy requirements.")
        .license("IBM Data De-Identification")
				.licenseUrl("https://github.com/Alvearie/de-identification/blob/master/LICENSE")
        .version("1.0.0")
        .build();
  }

  @Bean
  public Docket customImplementation() {
    String packages = "com.ibm.whc.deid.app.endpoint";
    String packages2 = "com.ibm.whc.deid.endpoint";
    // Include all controller in */api packages
    return new Docket(DocumentationType.SWAGGER_2).select()
        .apis(Predicates.or(RequestHandlerSelectors.basePackage(packages),
            RequestHandlerSelectors.basePackage(packages2)))
        .paths(PathSelectors.regex("/api.*")).build().apiInfo(apiInfo());
  }
}
