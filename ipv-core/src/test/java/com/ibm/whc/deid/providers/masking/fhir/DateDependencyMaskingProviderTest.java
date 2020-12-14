/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fhir;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ibm.whc.deid.ObjectMapperFactory;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import com.ibm.whc.deid.shared.pojo.config.json.JsonConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.DateDependencyMaskingProviderConfig;
import java.util.ArrayList;
import java.util.List;
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

    ObjectMapper mapper = ObjectMapperFactory.getObjectMapper();
    JsonNode originalNode = mapper.readTree(identifierJSON);

    DateDependencyMaskingProviderConfig maskingConfiguration =
        new DateDependencyMaskingProviderConfig();
    maskingConfiguration.setDatetimeYearDeleteNIntervalCompareDate("deceasedDateTime");
    
    DateDependencyMaskingProvider maskingProvider =
        new DateDependencyMaskingProvider(maskingConfiguration, buildMaskingConfig());     
    
    List<MaskingActionInputIdentifier> maskingInputs = buildMaskingInputs(originalNode, "birthDate");
    maskingProvider.maskIdentifierBatch(maskingInputs);
    JsonNode maskedNode = getParentNodeFromMaskingInputs(maskingInputs);
      
    assertEquals("08/12", maskedNode.get("birthDate").asText());
  }

  @Test
  public void testMaskExceedRange() throws Exception {
    String identifierJSON = "{\n" + "      \"resourceType\": \"Patient\",\n"
        + "      \"id\": \"example\",\n" + "      \"active\": false,\n"
        + "      \"birthDate\": \"08-12-2003 00:02:00\",\n" + "      \"deceasedBoolean\": true,\n"
        + "      \"deceasedDateTime\": \"05-11-2011 00:00:00\"\n" + "    }";

    ObjectMapper mapper = ObjectMapperFactory.getObjectMapper();
    JsonNode originalNode = mapper.readTree(identifierJSON);

    DateDependencyMaskingProviderConfig maskingConfiguration =
        new DateDependencyMaskingProviderConfig();
    maskingConfiguration.setDatetimeYearDeleteNIntervalCompareDate("deceasedDateTime");
    maskingConfiguration.setDateYearDeleteNDaysValue(500);

    DateDependencyMaskingProvider maskingProvider =
        new DateDependencyMaskingProvider(maskingConfiguration, buildMaskingConfig()); 

    List<MaskingActionInputIdentifier> maskingInputs = buildMaskingInputs(originalNode, "birthDate");
    maskingProvider.maskIdentifierBatch(maskingInputs);
    JsonNode maskedNode = getParentNodeFromMaskingInputs(maskingInputs);

    assertEquals("08-12-2003 00:02:00", maskedNode.get("birthDate").asText());
  }

  @Test
  public void testMaskDayAfterRange() throws Exception {
    String identifierJSON = "{\n" + "      \"resourceType\": \"Patient\",\n"
        + "      \"id\": \"example\",\n" + "      \"active\": false,\n"
        + "      \"birthDate\": \"22-12-2005 00:02:00\",\n" + "      \"deceasedBoolean\": true,\n"
        + "      \"deceasedDateTime\": \"23-12-2006 00:02:00\"\n" + "    }";

    ObjectMapper mapper = ObjectMapperFactory.getObjectMapper();
    JsonNode originalNode = mapper.readTree(identifierJSON);

    DateDependencyMaskingProviderConfig maskingConfiguration =
        new DateDependencyMaskingProviderConfig();
    maskingConfiguration.setDatetimeYearDeleteNIntervalCompareDate("deceasedDateTime");
    maskingConfiguration.setDateYearDeleteNDaysValue(365);

    DateDependencyMaskingProvider maskingProvider =
        new DateDependencyMaskingProvider(maskingConfiguration, buildMaskingConfig()); 

    List<MaskingActionInputIdentifier> maskingInputs = buildMaskingInputs(originalNode, "birthDate");
    maskingProvider.maskIdentifierBatch(maskingInputs);
    JsonNode maskedNode = getParentNodeFromMaskingInputs(maskingInputs);

    assertEquals("22-12-2005 00:02:00", maskedNode.get("birthDate").asText());
  }

  @Test
  public void testMaskDayAfterRangeISO() throws Exception {
    String identifierJSON = "{\n" + "      \"resourceType\": \"Patient\",\n"
        + "      \"id\": \"example\",\n" + "      \"active\": false,\n"
        + "      \"birthDate\": \"2005-09-23T00:02:00-05:00\",\n" + "      \"deceasedBoolean\": true,\n"
        + "      \"deceasedDateTime\": \"2006-09-24T00:02:00-05:00\"\n" + "    }";

    ObjectMapper mapper = ObjectMapperFactory.getObjectMapper();
    JsonNode originalNode = mapper.readTree(identifierJSON);

    DateDependencyMaskingProviderConfig maskingConfiguration =
        new DateDependencyMaskingProviderConfig();
    maskingConfiguration.setDatetimeYearDeleteNIntervalCompareDate("deceasedDateTime");
    maskingConfiguration.setDateYearDeleteNDaysValue(365);

    DateDependencyMaskingProvider maskingProvider =
        new DateDependencyMaskingProvider(maskingConfiguration, buildMaskingConfig()); 

    List<MaskingActionInputIdentifier> maskingInputs = buildMaskingInputs(originalNode, "birthDate");
    maskingProvider.maskIdentifierBatch(maskingInputs);
    JsonNode maskedNode = getParentNodeFromMaskingInputs(maskingInputs);

    assertEquals("2005-09-23T05:02:00Z", maskedNode.get("birthDate").asText());
  }

  @Test
  public void testMaskDayEqualRange() throws Exception {
    String identifierJSON = "{\n" + "      \"resourceType\": \"Patient\",\n"
        + "      \"id\": \"example\",\n" + "      \"active\": false,\n"
        + "      \"birthDate\": \"22-12-2005 00:02:00\",\n" + "      \"deceasedBoolean\": true,\n"
        + "      \"deceasedDateTime\": \"22-12-2006 00:02:00\"\n" + "    }";

    ObjectMapper mapper = ObjectMapperFactory.getObjectMapper();
    JsonNode originalNode = mapper.readTree(identifierJSON);

    DateDependencyMaskingProviderConfig maskingConfiguration =
        new DateDependencyMaskingProviderConfig();
    maskingConfiguration.setDatetimeYearDeleteNIntervalCompareDate("deceasedDateTime");
    maskingConfiguration.setDateYearDeleteNDaysValue(365);

    DateDependencyMaskingProvider maskingProvider =
        new DateDependencyMaskingProvider(maskingConfiguration, buildMaskingConfig()); 

    List<MaskingActionInputIdentifier> maskingInputs = buildMaskingInputs(originalNode, "birthDate");
    maskingProvider.maskIdentifierBatch(maskingInputs);
    JsonNode maskedNode = getParentNodeFromMaskingInputs(maskingInputs);

    assertEquals("22/12", maskedNode.get("birthDate").asText());
  }

  @Test
  public void testMaskDayEqualRangeISO() throws Exception {
    String identifierJSON = "{\n" + "      \"resourceType\": \"Patient\",\n"
        + "      \"id\": \"example\",\n" + "      \"active\": false,\n"
        + "      \"birthDate\": \"2005-09-24T00:02:00-05:00\",\n" + "      \"deceasedBoolean\": true,\n"
        + "      \"deceasedDateTime\": \"2006-09-24T00:02:00-05:00\"\n" + "    }";

    ObjectMapper mapper = ObjectMapperFactory.getObjectMapper();
    JsonNode originalNode = mapper.readTree(identifierJSON);

    DateDependencyMaskingProviderConfig maskingConfiguration =
        new DateDependencyMaskingProviderConfig();
    maskingConfiguration.setDatetimeYearDeleteNIntervalCompareDate("deceasedDateTime");
    maskingConfiguration.setDateYearDeleteNDaysValue(365);

    DateDependencyMaskingProvider maskingProvider =
        new DateDependencyMaskingProvider(maskingConfiguration, buildMaskingConfig()); 

    List<MaskingActionInputIdentifier> maskingInputs = buildMaskingInputs(originalNode, "birthDate");
    maskingProvider.maskIdentifierBatch(maskingInputs);
    JsonNode maskedNode = getParentNodeFromMaskingInputs(maskingInputs);

    assertEquals("24/09", maskedNode.get("birthDate").asText());
  }

  @Test
  public void testMaskDayBeforeRange() throws Exception {
    String identifierJSON = "{\n" + "      \"resourceType\": \"Patient\",\n"
        + "      \"id\": \"example\",\n" + "      \"active\": false,\n"
        + "      \"birthDate\": \"21-12-2005 00:02:00\",\n" + "      \"deceasedBoolean\": true,\n"
        + "      \"deceasedDateTime\": \"20-12-2006 00:02:00\"\n" + "    }";

    ObjectMapper mapper = ObjectMapperFactory.getObjectMapper();
    JsonNode originalNode = mapper.readTree(identifierJSON);

    DateDependencyMaskingProviderConfig maskingConfiguration =
        new DateDependencyMaskingProviderConfig();
    maskingConfiguration.setDatetimeYearDeleteNIntervalCompareDate("deceasedDateTime");
    maskingConfiguration.setDateYearDeleteNDaysValue(365);

    DateDependencyMaskingProvider maskingProvider =
        new DateDependencyMaskingProvider(maskingConfiguration, buildMaskingConfig()); 

    List<MaskingActionInputIdentifier> maskingInputs = buildMaskingInputs(originalNode, "birthDate");
    maskingProvider.maskIdentifierBatch(maskingInputs);
    JsonNode maskedNode = getParentNodeFromMaskingInputs(maskingInputs);

    assertEquals("21/12", maskedNode.get("birthDate").asText());
  }

  @Test
  public void testMaskDayBeforeRangeISO() throws Exception {
    String identifierJSON = "{\n" + "      \"resourceType\": \"Patient\",\n"
        + "      \"id\": \"example\",\n" + "      \"active\": false,\n"
        + "      \"birthDate\": \"2005-09-25T00:02:00-05:00\",\n" + "      \"deceasedBoolean\": true,\n"
        + "      \"deceasedDateTime\": \"2006-09-24T00:02:00-05:00\"\n" + "    }";

    ObjectMapper mapper = ObjectMapperFactory.getObjectMapper();
    JsonNode originalNode = mapper.readTree(identifierJSON);

    DateDependencyMaskingProviderConfig maskingConfiguration =
        new DateDependencyMaskingProviderConfig();
    maskingConfiguration.setDatetimeYearDeleteNIntervalCompareDate("deceasedDateTime");
    maskingConfiguration.setDateYearDeleteNDaysValue(365);

    DateDependencyMaskingProvider maskingProvider =
        new DateDependencyMaskingProvider(maskingConfiguration, buildMaskingConfig()); 

    List<MaskingActionInputIdentifier> maskingInputs = buildMaskingInputs(originalNode, "birthDate");
    maskingProvider.maskIdentifierBatch(maskingInputs);
    JsonNode maskedNode = getParentNodeFromMaskingInputs(maskingInputs);

    assertEquals("25/09", maskedNode.get("birthDate").asText());
  }

  @Test
  public void testMaskDefaultRange() throws Exception {
    String identifierJSON = "{\n" + "      \"resourceType\": \"Patient\",\n"
        + "      \"id\": \"example\",\n" + "      \"active\": false,\n"
        + "      \"birthDate\": \"08-12-2010 00:02:00\",\n" + "      \"deceasedBoolean\": true,\n"
        + "      \"deceasedDateTime\": \"05-11-2011 00:00:00\"\n" + "    }";

    ObjectMapper mapper = ObjectMapperFactory.getObjectMapper();
    JsonNode originalNode = mapper.readTree(identifierJSON);

    DateDependencyMaskingProviderConfig maskingConfiguration =
        new DateDependencyMaskingProviderConfig();
    maskingConfiguration.setDatetimeYearDeleteNIntervalCompareDate("deceasedDateTime");

    DateDependencyMaskingProvider maskingProvider =
        new DateDependencyMaskingProvider(maskingConfiguration, buildMaskingConfig()); 

    List<MaskingActionInputIdentifier> maskingInputs = buildMaskingInputs(originalNode, "birthDate");
    maskingProvider.maskIdentifierBatch(maskingInputs);
    JsonNode maskedNode = getParentNodeFromMaskingInputs(maskingInputs);

    // Default range is 365 days
    assertEquals("08/12", maskedNode.get("birthDate").asText());
  }

  @Test
  public void testMaskReturnAbsoluteValue() throws Exception {
    String identifierJSON = "{\n" + "      \"resourceType\": \"Patient\",\n"
        + "      \"id\": \"example\",\n" + "      \"active\": false,\n"
        + "      \"birthDate\": \"08-12-2010 00:02:00\",\n" + "      \"deceasedBoolean\": true,\n"
        + "      \"deceasedDateTime\": \"25-01-2010 00:00:00\"\n" + "    }";

    ObjectMapper mapper = ObjectMapperFactory.getObjectMapper();
    JsonNode originalNode = mapper.readTree(identifierJSON);

    DateDependencyMaskingProviderConfig maskingConfiguration =
        new DateDependencyMaskingProviderConfig();
    maskingConfiguration.setDatetimeYearDeleteNIntervalCompareDate("deceasedDateTime");

    DateDependencyMaskingProvider maskingProvider =
        new DateDependencyMaskingProvider(maskingConfiguration, buildMaskingConfig()); 

    List<MaskingActionInputIdentifier> maskingInputs = buildMaskingInputs(originalNode, "birthDate");
    maskingProvider.maskIdentifierBatch(maskingInputs);
    JsonNode maskedNode = getParentNodeFromMaskingInputs(maskingInputs);

    // Should handle this with absolute value when deceasedDateTime is
    // before birthDate
    assertEquals("08/12", maskedNode.get("birthDate").asText());
  }

  @Test
  public void testMaskMissingCompareDateConfig() throws Exception {
    String identifierJSON = "{\n" + "      \"resourceType\": \"Patient\",\n"
        + "      \"id\": \"example\",\n" + "      \"active\": false,\n"
        + "      \"birthDate\": \"08-12-2010 00:02:00\",\n" + "      \"deceasedBoolean\": true,\n"
        + "      \"deceasedDateTime\": \"05-11-2011 00:00:00\"\n" + "    }";

    ObjectMapper mapper = ObjectMapperFactory.getObjectMapper();
    JsonNode originalNode = mapper.readTree(identifierJSON);

    DateDependencyMaskingProviderConfig maskingConfiguration =
        new DateDependencyMaskingProviderConfig();
    
    DateDependencyMaskingProvider maskingProvider =
        new DateDependencyMaskingProvider(maskingConfiguration, buildMaskingConfig()); 

    List<MaskingActionInputIdentifier> maskingInputs = buildMaskingInputs(originalNode, "birthDate");
    maskingProvider.maskIdentifierBatch(maskingInputs);
    JsonNode maskedNode = getParentNodeFromMaskingInputs(maskingInputs);

    // Should return no change
    assertEquals("08-12-2010 00:02:00", maskedNode.get("birthDate").asText());
  }

  @Test
  public void testMaskCompareDateNull() throws Exception {
    // specified as null
    String identifierJSON = "{\n" + "      \"resourceType\": \"Patient\",\n"
        + "      \"id\": \"example\",\n" + "      \"active\": false,\n"
        + "      \"birthDate\": \"08-12-2010 00:02:00\",\n" + "      \"deceasedBoolean\": true,\n"
        + "      \"deceasedDateTime\": null\n" + "    }";

    ObjectMapper mapper = ObjectMapperFactory.getObjectMapper();
    JsonNode originalNode = mapper.readTree(identifierJSON);

    DateDependencyMaskingProviderConfig maskingConfiguration =
        new DateDependencyMaskingProviderConfig();
    maskingConfiguration.setDatetimeYearDeleteNIntervalCompareDate("deceasedDateTime");

    DateDependencyMaskingProvider maskingProvider =
        new DateDependencyMaskingProvider(maskingConfiguration, buildMaskingConfig()); 

    List<MaskingActionInputIdentifier> maskingInputs = buildMaskingInputs(originalNode, "birthDate");
    maskingProvider.maskIdentifierBatch(maskingInputs);
    JsonNode maskedNode = getParentNodeFromMaskingInputs(maskingInputs);

    // Should return no change
    assertEquals("08-12-2010 00:02:00", maskedNode.get("birthDate").asText());
    
    // not specified at all
    identifierJSON = "{\n" + "      \"resourceType\": \"Patient\",\n"
        + "      \"id\": \"example\",\n" + "      \"active\": false,\n"
        + "      \"birthDate\": \"08-12-2010 00:02:00\",\n      \"deceasedBoolean\": true}";

    originalNode = mapper.readTree(identifierJSON);

    maskingProvider =
        new DateDependencyMaskingProvider(maskingConfiguration, buildMaskingConfig()); 

    maskingInputs = buildMaskingInputs(originalNode, "birthDate");
    maskingProvider.maskIdentifierBatch(maskingInputs);
    maskedNode = getParentNodeFromMaskingInputs(maskingInputs);

    // Should return no change
    assertEquals("08-12-2010 00:02:00", maskedNode.get("birthDate").asText());
  }

  @Test
  public void testMaskCompareDateEmpty() throws Exception {
    String identifierJSON = "{\n" + "      \"resourceType\": \"Patient\",\n"
        + "      \"id\": \"example\",\n" + "      \"active\": false,\n"
        + "      \"birthDate\": \"08-12-2010 00:02:00\",\n" + "      \"deceasedBoolean\": true,\n"
        + "      \"deceasedDateTime\": \"\"\n" + "    }";

    ObjectMapper mapper = ObjectMapperFactory.getObjectMapper();
    JsonNode originalNode = mapper.readTree(identifierJSON);

    DateDependencyMaskingProviderConfig maskingConfiguration =
        new DateDependencyMaskingProviderConfig();
    maskingConfiguration.setDatetimeYearDeleteNIntervalCompareDate("deceasedDateTime");

    DateDependencyMaskingProvider maskingProvider =
        new DateDependencyMaskingProvider(maskingConfiguration, buildMaskingConfig()); 

    List<MaskingActionInputIdentifier> maskingInputs = buildMaskingInputs(originalNode, "birthDate");
    maskingProvider.maskIdentifierBatch(maskingInputs);
    JsonNode maskedNode = getParentNodeFromMaskingInputs(maskingInputs);

    // Should return null per unspecified value handling
    assertNotNull(maskedNode.get("birthDate"));
    assertTrue(maskedNode.get("birthDate").isNull());
  }

  @Test
  public void testMaskCompareDateInvalid() throws Exception {
    String identifierJSON = "{\n" + "      \"resourceType\": \"Patient\",\n"
        + "      \"id\": \"example\",\n" + "      \"active\": false,\n"
        + "      \"birthDate\": \"08-12-2010 00:02:00\",\n" + "      \"deceasedBoolean\": true,\n"
        + "      \"deceasedDateTime\": \"notdate\"\n" + "    }";

    ObjectMapper mapper = ObjectMapperFactory.getObjectMapper();
    JsonNode originalNode = mapper.readTree(identifierJSON);

    DateDependencyMaskingProviderConfig maskingConfiguration =
        new DateDependencyMaskingProviderConfig();
    maskingConfiguration.setDatetimeYearDeleteNIntervalCompareDate("deceasedDateTime");

    DateDependencyMaskingProvider maskingProvider =
        new DateDependencyMaskingProvider(maskingConfiguration, buildMaskingConfig()); 

    List<MaskingActionInputIdentifier> maskingInputs = buildMaskingInputs(originalNode, "birthDate");
    maskingProvider.maskIdentifierBatch(maskingInputs);
    JsonNode maskedNode = getParentNodeFromMaskingInputs(maskingInputs);

    // Should return null per unspecified value handling
    assertNotNull(maskedNode.get("birthDate"));
    assertTrue(maskedNode.get("birthDate").isNull());
  }

  @Test
  public void testMaskMaskDateNull() throws Exception {
    // specified as null
    String identifierJSON = "{\n" + "      \"resourceType\": \"Patient\",\n"
        + "      \"id\": \"example\",\n" + "      \"active\": false,\n"
        + "      \"birthDate\": null,\n" + "      \"deceasedBoolean\": true,\n"
        + "      \"deceasedDateTime\": \"25-01-2010 00:00:00\"\n" + "    }";

    ObjectMapper mapper = ObjectMapperFactory.getObjectMapper();
    JsonNode originalNode = mapper.readTree(identifierJSON);

    DateDependencyMaskingProviderConfig maskingConfiguration =
        new DateDependencyMaskingProviderConfig();
    maskingConfiguration.setDatetimeYearDeleteNIntervalCompareDate("deceasedDateTime");

    DateDependencyMaskingProvider maskingProvider =
        new DateDependencyMaskingProvider(maskingConfiguration, buildMaskingConfig()); 

    List<MaskingActionInputIdentifier> maskingInputs = buildMaskingInputs(originalNode, "birthDate");
    maskingProvider.maskIdentifierBatch(maskingInputs);
    JsonNode maskedNode = getParentNodeFromMaskingInputs(maskingInputs);

    assertTrue(maskedNode.get("birthDate").isNull());

    // not supplied at all
    identifierJSON = "{\n" + "      \"resourceType\": \"Patient\",\n"
        + "      \"id\": \"example\",\n" + "      \"active\": false,\n"
        + "      \"deceasedBoolean\": true,\n"
        + "      \"deceasedDateTime\": \"25-01-2010 00:00:00\"\n" + "    }";

    originalNode = mapper.readTree(identifierJSON);

    maskingInputs = buildMaskingInputs(originalNode, "birthDate");
    maskingProvider.maskIdentifierBatch(maskingInputs);
    maskedNode = getParentNodeFromMaskingInputs(maskingInputs);

    // Should return no change
    assertNull(maskedNode.get("birthDate"));
  }

  @Test
  public void testMaskMaskDateEmpty() throws Exception {
    String identifierJSON = "{\n" + "      \"resourceType\": \"Patient\",\n"
        + "      \"id\": \"example\",\n" + "      \"active\": false,\n"
        + "      \"birthDate\": \"\",\n" + "      \"deceasedBoolean\": true,\n"
        + "      \"deceasedDateTime\": \"25-01-2010 00:00:00\"\n" + "    }";

    ObjectMapper mapper = ObjectMapperFactory.getObjectMapper();
    JsonNode originalNode = mapper.readTree(identifierJSON);

    DateDependencyMaskingProviderConfig maskingConfiguration =
        new DateDependencyMaskingProviderConfig();
    maskingConfiguration.setDatetimeYearDeleteNIntervalCompareDate("deceasedDateTime");

    DateDependencyMaskingProvider maskingProvider =
        new DateDependencyMaskingProvider(maskingConfiguration, buildMaskingConfig()); 

    List<MaskingActionInputIdentifier> maskingInputs = buildMaskingInputs(originalNode, "birthDate");
    maskingProvider.maskIdentifierBatch(maskingInputs);
    JsonNode maskedNode = getParentNodeFromMaskingInputs(maskingInputs);

    // Should return null per unspecified value handling
    assertNotNull(maskedNode.get("birthDate"));
    assertTrue(maskedNode.get("birthDate").isNull());
  }

  @Test
  public void testMaskMaskDateInvalid() throws Exception {
    String identifierJSON = "{\n" + "      \"resourceType\": \"Patient\",\n"
        + "      \"id\": \"example\",\n" + "      \"active\": false,\n"
        + "      \"birthDate\": \"notdate\",\n" + "      \"deceasedBoolean\": true,\n"
        + "      \"deceasedDateTime\": \"25-01-2010 00:00:00\"\n" + "    }";

    ObjectMapper mapper = ObjectMapperFactory.getObjectMapper();
    JsonNode originalNode = mapper.readTree(identifierJSON);

    DateDependencyMaskingProviderConfig maskingConfiguration =
        new DateDependencyMaskingProviderConfig();
    maskingConfiguration.setDatetimeYearDeleteNIntervalCompareDate("deceasedDateTime");

    DateDependencyMaskingProvider maskingProvider =
        new DateDependencyMaskingProvider(maskingConfiguration, buildMaskingConfig()); 

    List<MaskingActionInputIdentifier> maskingInputs = buildMaskingInputs(originalNode, "birthDate");
    maskingProvider.maskIdentifierBatch(maskingInputs);
    JsonNode maskedNode = getParentNodeFromMaskingInputs(maskingInputs);

    // Should return null per unspecified value handling
    assertNotNull(maskedNode.get("birthDate"));
    assertTrue(maskedNode.get("birthDate").isNull());
  }

  private DeidMaskingConfig buildMaskingConfig() {
    DeidMaskingConfig config = new DeidMaskingConfig();
    config.setJson(new JsonConfig());
    return config;
  }
  
  private List<MaskingActionInputIdentifier> buildMaskingInputs(JsonNode originalNode, String maskedProperty) {
    List<MaskingActionInputIdentifier> list = new ArrayList<>();
    list.add(new MaskingActionInputIdentifier(null, originalNode.get(maskedProperty), originalNode, maskedProperty, null, null, null));
    return list;
  }
  
  private JsonNode getParentNodeFromMaskingInputs(List<MaskingActionInputIdentifier> maskingInputs) {
    return maskingInputs.get(0).getParent();
  }    
}
