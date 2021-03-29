/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fhir;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertTrue;
import java.io.InputStream;
import java.util.Arrays;
import java.util.List;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ibm.whc.deid.ObjectMapperFactory;
import com.ibm.whc.deid.providers.masking.BasicMaskingProviderFactory;
import com.ibm.whc.deid.providers.masking.MaskingProviderFactory;
import com.ibm.whc.deid.shared.pojo.config.ConfigSchemaType;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import com.ibm.whc.deid.shared.util.ConfigGenerator;
import scala.Tuple2;

public class GenericMaskingProviderTest {

  private MaskingProviderFactory maskingProviderFactory = new BasicMaskingProviderFactory();

  private final String tenantId = "TEST_TENANT";

  private static final Logger logger = LoggerFactory.getLogger(GenericMaskingProviderTest.class);

  @Test
  public void testDevices() throws Exception {
    String[] filenames = new String[] {"/fhir/examples/device-example-f001-feedingtube.json",
        "/fhir/examples/device-example-ihe-pcd.json",
        "/fhir/examples/device-example-pacemaker.json",
        "/fhir/examples/device-example-software.json", "/fhir/examples/device-example-udi1.json",
        "/fhir/examples/device-example.json", "/fhir/examples/patient-example-a.json",};

    DeidMaskingConfig defaultFhirConfig = (new ConfigGenerator()).getTestDeidConfig();

    GenericMaskingProvider genericMaskingProvider =
        new GenericMaskingProvider(defaultFhirConfig, maskingProviderFactory, tenantId);
    ObjectMapper mapper = new ObjectMapper();

    for (String filename : filenames) {
      logger.info("Processing: " + filename);
      try (InputStream is = this.getClass().getResourceAsStream(filename)) {
        JsonNode node = mapper.readTree(is);
        List<Tuple2<String, JsonNode>> resultList = genericMaskingProvider
            .maskJsonNode(Arrays.asList(new Tuple2<String, JsonNode>("123", node)));
        assertTrue(1 == resultList.size());
      }
    }
  }

  @Test
  public void testPatientE2E() throws Exception {
    String maskingRules = null;
    String maskingProviders = null;
    try (
        InputStream maskingRulesStream =
            this.getClass().getResourceAsStream("/config/generic/patient_masking_rules.json");
        InputStream maskingProvidersStream =
            this.getClass().getResourceAsStream("/config/patient_masking_providers.json");) {
      maskingRules = ConfigGenerator.readResourceFileAsString(maskingRulesStream);
      maskingProviders = ConfigGenerator.readResourceFileAsString(maskingProvidersStream);
    }

    DeidMaskingConfig config = ConfigGenerator.getDeidConfig(maskingRules, maskingProviders);
    config.setDefaultNoRuleResolution(true);
    config.getJson().setSchemaType(ConfigSchemaType.GEN);
    GenericMaskingProvider genericMaskingProvider =
        new GenericMaskingProvider(config, maskingProviderFactory, tenantId);

    ObjectMapper objectMapper = new ObjectMapper();
    logger.info(objectMapper.writeValueAsString(config));

    ObjectMapper mapper = ObjectMapperFactory.getObjectMapper();

    String filename = "/fhir/patientExample.json";

    System.out.println("Processing: " + filename);
    JsonNode node;
    try (InputStream is = this.getClass().getResourceAsStream(filename)) {
      node = mapper.readTree(is);
    }
    JsonNode identifier = node.get("identifier").iterator().next();
    String originalPeriodStart = identifier.get("period").get("start").asText();
    assertEquals("2001-05-06", originalPeriodStart);
    assertEquals("12345", identifier.get("value").asText());
    assertEquals("Acme Healthcare", identifier.get("assigner").get("display").asText());

    String result = mapper.writeValueAsString(genericMaskingProvider
        .maskJsonNode(Arrays.asList(new Tuple2<String, JsonNode>("123", node))).get(0)._2);
    JsonNode resultNode = mapper.readTree(result);
    JsonNode maskedIdentifier = resultNode.get("identifier").iterator().next();
    String maskedPeriodStart = maskedIdentifier.get("period").get("start").asText();

    assertNotEquals("2001-05-06", maskedPeriodStart);
    assertTrue(maskedIdentifier.get("value").isNull());
  }
}
