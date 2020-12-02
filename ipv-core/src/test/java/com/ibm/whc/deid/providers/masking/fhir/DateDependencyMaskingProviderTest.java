/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fhir;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import com.ibm.whc.deid.shared.pojo.config.json.JsonConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.DateDependencyMaskingProviderConfig;
import org.junit.Test;

/**
 * Unit tests for date dependency masking provider
 *
 */
public class DateDependencyMaskingProviderTest {
  @Test
  public void testMaskWithinRange() throws Exception {
    String identifierJSON = "{\n" + "      \"resourceType\": \"Patient\",\n"
        + "      \"id\": \"example\",\n" + "      \"active\": false,\n"
        + "      \"birthDate\": \"08-12-2010 00:02:00\",\n" + "      \"deceasedBoolean\": true,\n"
        + "      \"deceasedDateTime\": \"05-11-2011 00:00:00\"\n" + "    }";

    ObjectMapper mapper = new ObjectMapper();
    JsonNode originalNode = mapper.readTree(identifierJSON);

    DateDependencyMaskingProviderConfig maskingConfiguration =
        new DateDependencyMaskingProviderConfig();

    maskingConfiguration.setDatetimeYearDeleteNIntervalMaskDate("birthDate");
    maskingConfiguration.setDatetimeYearDeleteNIntervalCompareDate("deceasedDateTime");
    DateDependencyMaskingProvider maskingProvider =
        new DateDependencyMaskingProvider(maskingConfiguration, buildMaskingConfig()); 

    JsonNode maskedNode = maskingProvider.mask(originalNode);

    assertTrue(maskedNode.get("birthDate").asText().equals("08/12"));
  }

  @Test
  public void testMaskExceedRange() throws Exception {
    String identifierJSON = "{\n" + "      \"resourceType\": \"Patient\",\n"
        + "      \"id\": \"example\",\n" + "      \"active\": false,\n"
        + "      \"birthDate\": \"08-12-2003 00:02:00\",\n" + "      \"deceasedBoolean\": true,\n"
        + "      \"deceasedDateTime\": \"05-11-2011 00:00:00\"\n" + "    }";

    ObjectMapper mapper = new ObjectMapper();
    JsonNode originalNode = mapper.readTree(identifierJSON);

    DateDependencyMaskingProviderConfig maskingConfiguration =
        new DateDependencyMaskingProviderConfig();

    maskingConfiguration.setDatetimeYearDeleteNIntervalMaskDate("birthDate");
    maskingConfiguration.setDatetimeYearDeleteNIntervalCompareDate("deceasedDateTime");
    maskingConfiguration.setDateYearDeleteNDaysValue(500);

    DateDependencyMaskingProvider maskingProvider =
        new DateDependencyMaskingProvider(maskingConfiguration, buildMaskingConfig()); 

    JsonNode maskedNode = maskingProvider.mask(originalNode);

    assertTrue(maskedNode.get("birthDate").asText().equals("08-12-2003 00:02:00"));
  }

  @Test
  public void testMaskDefaultRange() throws Exception {
    String identifierJSON = "{\n" + "      \"resourceType\": \"Patient\",\n"
        + "      \"id\": \"example\",\n" + "      \"active\": false,\n"
        + "      \"birthDate\": \"08-12-2010 00:02:00\",\n" + "      \"deceasedBoolean\": true,\n"
        + "      \"deceasedDateTime\": \"05-11-2011 00:00:00\"\n" + "    }";

    ObjectMapper mapper = new ObjectMapper();
    JsonNode originalNode = mapper.readTree(identifierJSON);

    DateDependencyMaskingProviderConfig maskingConfiguration =
        new DateDependencyMaskingProviderConfig();

    maskingConfiguration.setDatetimeYearDeleteNIntervalMaskDate("birthDate");
    maskingConfiguration.setDatetimeYearDeleteNIntervalCompareDate("deceasedDateTime");

    DateDependencyMaskingProvider maskingProvider =
        new DateDependencyMaskingProvider(maskingConfiguration, buildMaskingConfig()); 

    JsonNode maskedNode = maskingProvider.mask(originalNode);

    // Default range is 365 days
    assertTrue(maskedNode.get("birthDate").asText().equals("08/12"));
  }

  @Test
  public void testMaskReturnAbsoluteValue() throws Exception {
    String identifierJSON = "{\n" + "      \"resourceType\": \"Patient\",\n"
        + "      \"id\": \"example\",\n" + "      \"active\": false,\n"
        + "      \"birthDate\": \"08-12-2010 00:02:00\",\n" + "      \"deceasedBoolean\": true,\n"
        + "      \"deceasedDateTime\": \"25-01-2010 00:00:00\"\n" + "    }";

    ObjectMapper mapper = new ObjectMapper();
    JsonNode originalNode = mapper.readTree(identifierJSON);

    DateDependencyMaskingProviderConfig maskingConfiguration =
        new DateDependencyMaskingProviderConfig();

    maskingConfiguration.setDatetimeYearDeleteNIntervalMaskDate("birthDate");
    maskingConfiguration.setDatetimeYearDeleteNIntervalCompareDate("deceasedDateTime");

    DateDependencyMaskingProvider maskingProvider =
        new DateDependencyMaskingProvider(maskingConfiguration, buildMaskingConfig()); 

    JsonNode maskedNode = maskingProvider.mask(originalNode);

    // Should handle this with absolute value when deceasedDateTime is
    // before birthDate
    assertTrue(maskedNode.get("birthDate").asText().equals("08/12"));
  }

  @Test
  public void testMaskMissingCompareDate() throws Exception {
    String identifierJSON = "{\n" + "      \"resourceType\": \"Patient\",\n"
        + "      \"id\": \"example\",\n" + "      \"active\": false,\n"
        + "      \"birthDate\": \"08-12-2010 00:02:00\",\n" + "      \"deceasedBoolean\": true,\n"
        + "      \"deceasedDateTime\": \"05-11-2011 00:00:00\"\n" + "    }";

    ObjectMapper mapper = new ObjectMapper();
    JsonNode originalNode = mapper.readTree(identifierJSON);

    DateDependencyMaskingProviderConfig maskingConfiguration =
        new DateDependencyMaskingProviderConfig();

    maskingConfiguration.setDatetimeYearDeleteNIntervalMaskDate("birthDate");

    DateDependencyMaskingProvider maskingProvider =
        new DateDependencyMaskingProvider(maskingConfiguration, buildMaskingConfig()); 

    JsonNode maskedNode = maskingProvider.mask(originalNode);

    // Should return null
    assertEquals(null, maskedNode);
  }
  
  private DeidMaskingConfig buildMaskingConfig() {
    DeidMaskingConfig config = new DeidMaskingConfig();
    config.setJson(new JsonConfig());
    return config;
  }
    
}
