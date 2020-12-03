/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.app.endpoint;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.info.License;

/**
 * Use @Configuration so that Spring creates a Spring bean in the application context
 *
 */
@Configuration
public class SwaggerConfiguration {

  Info info() {
    return new Info().title("IBM Data De-Identification").description(
        "The Data De-Identification service provides a wide range of de-identification capabilities\r\n"
            + "    // designed to support GDPR, HIPAA, CCPA and other privacy frameworks allowing customers to meet\r\n"
            + "    // their regulatory and privacy requirements.")
        .license(new License().name("IBM Data De-Identification")
            .url("https://github.com/Alvearie/de-identification/blob/master/LICENSE"))
        .version("1.0.0");
  }

  @Bean
  public OpenAPI customImplementation() {
    // Scan the following directories as described in the application.properties file
    return new OpenAPI().components(new Components()).info(info());
  }
}
