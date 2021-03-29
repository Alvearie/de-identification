/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fhir;

import static org.hamcrest.CoreMatchers.not;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import java.io.InputStream;
import java.util.Arrays;
import java.util.List;
import java.util.stream.IntStream;
import org.junit.Ignore;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ibm.whc.deid.ObjectMapperFactory;
import com.ibm.whc.deid.providers.masking.MaskingProviderFactory;
import com.ibm.whc.deid.providers.masking.MaskingProviderFactoryUtil;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import com.ibm.whc.deid.shared.pojo.config.json.JsonConfig;
import com.ibm.whc.deid.shared.pojo.config.json.JsonMaskingRule;
import com.ibm.whc.deid.shared.util.ConfigGenerator;
import scala.Tuple2;

public class FHIRMaskingProviderTest {

  private static final Logger logger = LoggerFactory.getLogger(FHIRMaskingProviderTest.class);

  private final String tenantId = "TEST_TENANT";

  private MaskingProviderFactory maskingProviderFactory =
      MaskingProviderFactoryUtil.getMaskingProviderFactory();

  @Test
  public void testLoadRulesForResource() {
    DeidMaskingConfig config = new DeidMaskingConfig();
    assertNull(config.getJson());
    List<FHIRResourceField> list = FHIRMaskingProvider.loadRulesForResource("x", config, "/");
    assertNotNull(list);
    assertEquals(0, list.size());

    config.setJson(new JsonConfig());
    config.getJson().getMaskingRules().add(new JsonMaskingRule("/fhir/path/data", "rule1"));
    assertEquals(1, config.getJson().getMaskingRules().size());
    list = FHIRMaskingProvider.loadRulesForResource("x", config, "/");
    assertNotNull(list);
    assertEquals(0, list.size());
    list = FHIRMaskingProvider.loadRulesForResource("path", config, "/fhir/");
    assertNotNull(list);
    assertEquals(1, list.size());
    assertEquals("/fhir/path/data", list.get(0).getKey());
    assertEquals("rule1", list.get(0).getShortRuleName());

    config.setJson(null);
    assertNull(config.getJson());
    list = FHIRMaskingProvider.loadRulesForResource("x", config, "/");
    assertNotNull(list);
    assertEquals(0, list.size());

    config.setJson(new JsonConfig());
    assertNotNull(config.getJson());
    assertNotNull(config.getJson().getMaskingRules());
    assertEquals(0, config.getJson().getMaskingRules().size());
    list = FHIRMaskingProvider.loadRulesForResource("x", config, "/");
    assertNotNull(list);
    assertEquals(0, list.size());

    config = new ConfigGenerator().getTestDeidConfig();
    String basePathPrefix = "/fhir/";
    assertEquals(39,
        FHIRMaskingProvider.loadRulesForResource("Device", config, basePathPrefix).size());
  }

  @Ignore
  @Test
  public void testMaintainsDataType() throws Exception {
    DeidMaskingConfig defaultFhirConfig = (new ConfigGenerator()).getTestDeidConfig();
    FHIRMaskingProvider fhirMaskingProvider =
        new FHIRMaskingProvider(defaultFhirConfig, maskingProviderFactory, tenantId);

    try (
        InputStream is = this.getClass().getResourceAsStream("/fhir/MedicationOrder-230986.json")) {
      JsonNode node = ObjectMapperFactory.getObjectMapper().readTree(is);

      assertTrue(node.get("dispenseRequest").get("numberOfRepeatsAllowed").isInt());
      assertEquals(2, node.get("dispenseRequest").get("numberOfRepeatsAllowed").intValue());

      ObjectMapper mapper = ObjectMapperFactory.getObjectMapper();
      // We are using random to mask the value. to avoid the masked value
      // happens to be the original, try 10 times.
      String masked = mapper.writeValueAsString(IntStream.range(0, 10)
          .mapToObj(x -> fhirMaskingProvider
              .maskJsonNode(Arrays.asList(new Tuple2<String, JsonNode>("123", node))))
          .filter(maskedStr -> {
            JsonNode maskedNode = maskedStr.get(0)._2;
            int numberOfRepeatsAllowed =
                maskedNode.get("dispenseRequest").get("numberOfRepeatsAllowed").intValue();
            return numberOfRepeatsAllowed != 2;
          }).findFirst().get().get(0)._2);
      JsonNode maskedNode = ObjectMapperFactory.getObjectMapper().readTree(masked);

      assertTrue(maskedNode.get("dispenseRequest").get("numberOfRepeatsAllowed").intValue() != 2);

      assertTrue(maskedNode.get("dispenseRequest").get("numberOfRepeatsAllowed").isInt());
    }
  }

  @Test
  public void test239202() throws Exception {
    DeidMaskingConfig defaultFhirConfig = (new ConfigGenerator()).getTestDeidConfig();
    FHIRMaskingProvider fhirMaskingProvider =
        new FHIRMaskingProvider(defaultFhirConfig, maskingProviderFactory, tenantId);
    try (InputStream is =
        this.getClass().getResourceAsStream("/fhir/MedicationAdministration-239202.json")) {
      JsonNode node = ObjectMapperFactory.getObjectMapper().readTree(is);
      JsonNode maskedNode = fhirMaskingProvider
          .maskJsonNode(Arrays.asList(new Tuple2<String, JsonNode>("123", node))).get(0)._2();

      assertTrue(maskedNode.get("practitioner").get("reference").asText().length() > 0);
    }
  }

  @Ignore
  @Test
  public void testMaintainsDataTypeArrays() throws Exception {
    DeidMaskingConfig defaultFhirConfig = (new ConfigGenerator()).getTestDeidConfig();
    FHIRMaskingProvider fhirMaskingProvider =
        new FHIRMaskingProvider(defaultFhirConfig, maskingProviderFactory, tenantId);

    ObjectMapper mapper = ObjectMapperFactory.getObjectMapper();
    try (InputStream is =
        this.getClass().getResourceAsStream("/fhir/MedicationOrder-arrays-230986.json")) {
      JsonNode node = ObjectMapperFactory.getObjectMapper().readTree(is);

      assertTrue(node.get("dispenseRequest").get("numberOfRepeatsAllowed").isArray());
      assertEquals(2, node.get("dispenseRequest").get("numberOfRepeatsAllowed").get(0).intValue());

      // We are using random to mask the value. to avoid the masked value
      // happens to be the original, try 10 times
      String masked = mapper.writeValueAsString(IntStream.range(0, 10)
          .mapToObj(x -> fhirMaskingProvider
              .maskJsonNode(Arrays.asList(new Tuple2<String, JsonNode>("123", node))))
          .filter(maskedStr -> {
            JsonNode maskedNode = maskedStr.get(0)._2;
            int numberOfRepeatsAllowed =
                maskedNode.get("dispenseRequest").get("numberOfRepeatsAllowed").get(0).intValue();
            return numberOfRepeatsAllowed != 2;
          }).findFirst().get().get(0)._2);
      JsonNode maskedNode = ObjectMapperFactory.getObjectMapper().readTree(masked);

      assertTrue(
          maskedNode.get("dispenseRequest").get("numberOfRepeatsAllowed").get(0).intValue() != 2);

      assertTrue(maskedNode.get("dispenseRequest").get("numberOfRepeatsAllowed").get(0).isInt());
    }
  }

  @Test
  public void testDevices() throws Exception {
    String[] filenames = new String[] {"/fhir/examples/device-example-f001-feedingtube.json",
        "/fhir/examples/device-example-ihe-pcd.json",
        "/fhir/examples/device-example-pacemaker.json",
        "/fhir/examples/device-example-software.json", "/fhir/examples/device-example-udi1.json",
        "/fhir/examples/device-example.json", "/fhir/examples/patient-example-a.json",};

    DeidMaskingConfig defaultFhirConfig = (new ConfigGenerator()).getTestDeidConfig();
    FHIRMaskingProvider fhirMaskingProvider =
        new FHIRMaskingProvider(defaultFhirConfig, maskingProviderFactory, tenantId);
    ObjectMapper mapper = new ObjectMapper();

    for (String filename : filenames) {
      logger.info("Processing " + filename);
      try (InputStream is = this.getClass().getResourceAsStream(filename)) {
        JsonNode node = mapper.readTree(is);
        String original = node.asText();

        String masked = mapper.writeValueAsString(fhirMaskingProvider
            .maskJsonNode(Arrays.asList(new Tuple2<String, JsonNode>("123", node))).get(0)._2);
        assertThat(masked, not(original));
      }
    }
  }

  @Test
  public void testPatientE2E() throws Exception {
    String maskingRules = null;
    String maskingProviders = null;
    try (
        InputStream maskingRulesStream =
            this.getClass().getResourceAsStream("/config/fhir/patient_masking_rules.json");
        InputStream maskingProvidersStream =
            this.getClass().getResourceAsStream("/config/patient_masking_providers.json");) {
      maskingRules = ConfigGenerator.readResourceFileAsString(maskingRulesStream);
      maskingProviders = ConfigGenerator.readResourceFileAsString(maskingProvidersStream);
    }

    DeidMaskingConfig fhirConfig = ConfigGenerator.getDeidConfig(maskingRules, maskingProviders);
    fhirConfig.setDefaultNoRuleResolution(true);
    FHIRMaskingProvider fhirMaskingProvider =
        new FHIRMaskingProvider(fhirConfig, maskingProviderFactory, tenantId);

    ObjectMapper mapper = ObjectMapperFactory.getObjectMapper();

    String filename = "/fhir/patientExample.json";

    InputStream is = this.getClass().getResourceAsStream(filename);

    JsonNode node = mapper.readTree(is);
    JsonNode identifier = node.get("identifier").iterator().next();
    String originalPeriodStart = identifier.get("period").get("start").asText();
    assertEquals("2001-05-06", originalPeriodStart);
    assertEquals("12345", identifier.get("value").asText());
    assertEquals("Acme Healthcare", identifier.get("assigner").get("display").asText());

    String result = mapper.writeValueAsString(fhirMaskingProvider
        .maskJsonNode(Arrays.asList(new Tuple2<String, JsonNode>("123", node))).get(0)._2());
    JsonNode resultNode = mapper.readTree(result);
    JsonNode maskedIdentifier = resultNode.get("identifier").iterator().next();
    String maskedPeriodStart = maskedIdentifier.get("period").get("start").asText();

    assertNotEquals("2001-05-06", maskedPeriodStart);
    assertTrue(maskedIdentifier.get("value").isNull());
  }

  @Test
  public void testExamples() throws Exception {
    String[] filenames = new String[] {"/fhir/examples/bodysite-example.json",
        "/fhir/examples/careplan-example-GPVisit.json",
        "/fhir/examples/careplan-example-f001-heart.json",
        "/fhir/examples/careplan-example-f002-lung.json",
        "/fhir/examples/careplan-example-f003-pharynx.json",
        "/fhir/examples/careplan-example-f201-renal.json",
        "/fhir/examples/careplan-example-f202-malignancy.json",
        "/fhir/examples/careplan-example-f203-sepsis.json",
        "/fhir/examples/careplan-example-integrated.json",
        "/fhir/examples/careplan-example-pregnancy.json", "/fhir/examples/careplan-example.json",
        "/fhir/examples/contract-example.json",
        "/fhir/examples/device-example-f001-feedingtube.json",
        "/fhir/examples/device-example-ihe-pcd.json",
        "/fhir/examples/device-example-pacemaker.json",
        "/fhir/examples/device-example-software.json", "/fhir/examples/device-example-udi1.json",
        "/fhir/examples/device-example.json",
        "/fhir/examples/devicecomponent-example-prodspec.json",
        "/fhir/examples/devicecomponent-example.json", "/fhir/examples/devicemetric-example.json",
        "/fhir/examples/goal-example.json", "/fhir/examples/group-example-member.json",
        "/fhir/examples/group-example.json", "/fhir/examples/location-example-ambulance.json",
        "/fhir/examples/location-example-hl7hq.json",
        "/fhir/examples/location-example-patients-home.json",
        "/fhir/examples/location-example-room.json",
        "/fhir/examples/location-example-ukpharmacy.json", "/fhir/examples/location-example.json",
        "/fhir/examples/location-extensions-Location-alias.canonical.json",
        "/fhir/examples/location-extensions-Location-alias.json",
        "/fhir/examples/medication-example-f001-combivent.json",
        "/fhir/examples/medication-example-f002-crestor.json",
        "/fhir/examples/medication-example-f003-tolbutamide.json",
        "/fhir/examples/medication-example-f004-metoprolol.json",
        "/fhir/examples/medication-example-f005-enalapril.json",
        "/fhir/examples/medication-example-f201-salmeterol.json",
        "/fhir/examples/medication-example-f202-flucloxacilline.json",
        "/fhir/examples/medication-example-f203-paracetamol.json",
        "/fhir/examples/medicationadministrationexample1.json",
        "/fhir/examples/medicationadministrationexample2.json",
        "/fhir/examples/medicationadministrationexample3.json",
        "/fhir/examples/medicationorder-example-f001-combivent.json",
        "/fhir/examples/medicationorder-example-f002-crestor.json",
        "/fhir/examples/medicationorder-example-f003-tolbutamide.json",
        "/fhir/examples/medicationorder-example-f004-metoprolol.json",
        "/fhir/examples/medicationorder-example-f005-enalapril.json",
        "/fhir/examples/medicationorder-example-f201-salmeterol.json",
        "/fhir/examples/medicationorder-example-f202-flucloxacilline.json",
        "/fhir/examples/medicationorder-example-f203-paracetamol.json",
        "/fhir/examples/observation-example-bloodpressure-cancel.json",
        "/fhir/examples/observation-example-bloodpressure.json",
        "/fhir/examples/observation-example-f001-glucose.json",
        "/fhir/examples/observation-example-f002-excess.json",
        "/fhir/examples/observation-example-f003-co2.json",
        "/fhir/examples/observation-example-f004-erythrocyte.json",
        "/fhir/examples/observation-example-f005-hemoglobin.json",
        "/fhir/examples/observation-example-f202-temperature.json",
        "/fhir/examples/observation-example-f203-bicarbonate.json",
        "/fhir/examples/observation-example-f204-creatinine.json",
        "/fhir/examples/observation-example-f205-egfr.json",
        "/fhir/examples/observation-example-f206-staphylococcus.json",
        "/fhir/examples/observation-example-genetics-1.json",
        "/fhir/examples/observation-example-genetics-2.json",
        "/fhir/examples/observation-example-genetics-3.json",
        "/fhir/examples/observation-example-genetics-4.json",
        "/fhir/examples/observation-example-genetics-5.json",
        "/fhir/examples/observation-example-glasgow-qa.json",
        "/fhir/examples/observation-example-glasgow.json",
        "/fhir/examples/observation-example-sample-data.json",
        "/fhir/examples/observation-example-satO2.json",
        "/fhir/examples/observation-example-unsat.json", "/fhir/examples/observation-example.json",
        "/fhir/examples/patient-example-a.json", "/fhir/examples/patient-example-animal.json",
        "/fhir/examples/patient-example-b.json", "/fhir/examples/patient-example-c.json",
        "/fhir/examples/patient-example-d.json", "/fhir/examples/patient-example-dicom.json",
        "/fhir/examples/patient-example-f001-pieter.json",
        "/fhir/examples/patient-example-f201-roel.json",
        "/fhir/examples/patient-example-ihe-pcd.json",
        "/fhir/examples/patient-example-proband.json",
        "/fhir/examples/patient-example-us-extensions.json",
        "/fhir/examples/patient-example-xcda.json", "/fhir/examples/patient-example-xds.json",
        "/fhir/examples/patient-example.json",
        "/fhir/examples/patient-examples-cypress-template.json",
        "/fhir/examples/patient-examples-general.json",
        "/fhir/examples/practitioner-example-f001-evdb.json",
        "/fhir/examples/practitioner-example-f002-pv.json",
        "/fhir/examples/practitioner-example-f003-mv.json",
        "/fhir/examples/practitioner-example-f004-rb.json",
        "/fhir/examples/practitioner-example-f005-al.json",
        "/fhir/examples/practitioner-example-f006-rvdb.json",
        "/fhir/examples/practitioner-example-f007-sh.json",
        "/fhir/examples/practitioner-example-f201-ab.json",
        "/fhir/examples/practitioner-example-f202-lm.json",
        "/fhir/examples/practitioner-example-f203-jvg.json",
        "/fhir/examples/practitioner-example-f204-ce.json",
        "/fhir/examples/practitioner-example-xcda-author.json",
        "/fhir/examples/practitioner-example-xcda1.json",
        "/fhir/examples/practitioner-example.json",
        "/fhir/examples/practitioner-examples-general.json",
        "/fhir/examples/questionnaire-example-bluebook.json",
        "/fhir/examples/questionnaire-example-f201-lifelines.json",
        "/fhir/examples/questionnaire-example-gcs.json",
        "/fhir/examples/questionnaire-example.json",
        "/fhir/examples/questionnaireresponse-example-bluebook.json",
        "/fhir/examples/questionnaireresponse-example-f201-lifelines.json",
        "/fhir/examples/questionnaireresponse-example-gcs.json",
        "/fhir/examples/questionnaireresponse-example.json",
        "/fhir/examples/auditevent-example-disclosure.json",
        "/fhir/examples/auditevent-example.json",
        "/fhir/examples/organization-example-f001-burgers.json",
        "/fhir/examples/organization-example-f002-burgers-card.json",
        "/fhir/examples/organization-example-f003-burgers-ENT.json",
        "/fhir/examples/organization-example-f201-aumc.json",
        "/fhir/examples/organization-example-f203-bumc.json",
        "/fhir/examples/organization-example-gastro.json",
        "/fhir/examples/organization-example-good-health-care.json",
        "/fhir/examples/organization-example-insurer.json",
        "/fhir/examples/organization-example-lab.json", "/fhir/examples/organization-example.json"};

    DeidMaskingConfig defaultFhirConfig = (new ConfigGenerator()).getTestDeidConfig();
    FHIRMaskingProvider fhirMaskingProvider =
        new FHIRMaskingProvider(defaultFhirConfig, maskingProviderFactory, tenantId);

    ObjectMapper mapper = new ObjectMapper();

    for (String filename : filenames) {
      InputStream is = this.getClass().getResourceAsStream(filename);
      JsonNode node = mapper.readTree(is);

      List<Tuple2<String, JsonNode>> resultList = fhirMaskingProvider
          .maskJsonNode(Arrays.asList(new Tuple2<String, JsonNode>("123", node)));
      assertTrue(1 == resultList.size());
    }
  }
}
