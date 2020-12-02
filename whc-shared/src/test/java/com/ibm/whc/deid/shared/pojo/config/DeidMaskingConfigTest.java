/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ibm.whc.deid.shared.pojo.config.json.JsonConfig;
import com.ibm.whc.deid.shared.pojo.config.json.JsonMaskingRule;
import com.ibm.whc.deid.shared.pojo.config.masking.HashMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.RedactMaskingProviderConfig;
import com.ibm.whc.deid.shared.util.ConfigGenerator;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class DeidMaskingConfigTest {

  private static final Logger log = LoggerFactory.getLogger(DeidMaskingConfigTest.class);

  @Test
  public void testRules() {
    DeidMaskingConfig config = new DeidMaskingConfig();
    assertNull(config.getRules());
    assertNull(config.getRulesMap());
    
    List<Rule> rules = new ArrayList<>();
    rules.add(new Rule("rule1", Arrays.asList(new HashMaskingProviderConfig(), new RedactMaskingProviderConfig())));
    config.setRules(rules);
    assertNotNull(config.getRules());
    assertEquals(1, config.getRules().size());
    assertEquals("rule1", config.getRules().get(0).getName());
    assertEquals(2, config.getRules().get(0).getMaskingProviders().size());
    assertNotNull(config.getRulesMap());
    assertEquals(1, config.getRulesMap().size());
    assertNotNull(config.getRulesMap().get("rule1"));
    assertEquals(2, config.getRulesMap().get("rule1").getMaskingProviders().size());
    assertNull(config.getRulesMap().get("Rule1"));

    rules.add(null);
    config.setRules(rules);
    assertNotNull(config.getRules());
    assertEquals(2, config.getRules().size());
    assertEquals("rule1", config.getRules().get(0).getName());
    assertEquals(2, config.getRules().get(0).getMaskingProviders().size());
    assertNull(config.getRules().get(1));
    assertNotNull(config.getRulesMap());
    assertEquals(1, config.getRulesMap().size());
    assertNotNull(config.getRulesMap().get("rule1"));
    assertEquals(2, config.getRulesMap().get("rule1").getMaskingProviders().size());
    assertNull(config.getRulesMap().get("Rule1"));

    config.setRules(null);
    assertNull(config.getRules());
    assertNotNull(config.getRulesMap());   
    assertEquals(0, config.getRulesMap().size());
  }

  @Test
  public void testGetStringValueWithPrefixMatch() {
    DeidMaskingConfig config = new DeidMaskingConfig();
    assertNull(config.getJson());
    Map<String,String> map = config.getStringValueWithPrefixMatch("x");
    assertNotNull(map);
    assertEquals(0, map.size());

    config.setJson(new JsonConfig());
    config.getJson().getMaskingRules().add(new JsonMaskingRule("/fhir/path/data", "rule1"));
    assertEquals(1, config.getJson().getMaskingRules().size());
    map = config.getStringValueWithPrefixMatch("x");
    assertNotNull(map);
    assertEquals(0, map.size());
    map = config.getStringValueWithPrefixMatch("/fhir/path");
    assertNotNull(map);
    assertEquals(1, map.size());
    assertEquals("rule1", map.get("/fhir/path/data"));
    
    config.setJson(null);
    assertNull(config.getJson());
    map = config.getStringValueWithPrefixMatch("x");
    assertNotNull(map);
    assertEquals(0, map.size());

    config.setJson(new JsonConfig());
    assertNotNull(config.getJson());
    assertNotNull(config.getJson().getMaskingRules());
    assertEquals(0, config.getJson().getMaskingRules().size());
    map = config.getStringValueWithPrefixMatch("x");
    assertNotNull(map);
    assertEquals(0, map.size());
  }

  /**
   * Create a default deid masking configuration. Serialize to String, de-serialize to POJO, and
   * serialize again to make sure the two strings are the same.
   *
   * @throws Exception
   */
  @Test
  public void testSerializeDedeserialize() throws Exception {
    ObjectMapper objectMapper = new ObjectMapper();
    
    DeidMaskingConfig maskingConfig = (new ConfigGenerator()).getTestDeidConfig();

    String maskingConfigStr = objectMapper.writeValueAsString(maskingConfig);
    log.info(maskingConfigStr);

    DeidMaskingConfig maskingConfig2 =
        objectMapper.readValue(maskingConfigStr, DeidMaskingConfig.class);

    String maskingConfigStr2 = objectMapper.writeValueAsString(maskingConfig2);

    log.info(maskingConfigStr2);
    assertEquals(maskingConfigStr, maskingConfigStr2);
  }

  @Test
  public void testDeserailize() throws IOException {
    ObjectMapper objectMapper = new ObjectMapper();

    Path path = Paths.get("src/test/resources/deid_masking_config.json");

    String maskingConfigStr = Files.readAllLines(path).stream().collect(Collectors.joining());

    DeidMaskingConfig maskingConfig =
        objectMapper.readValue(maskingConfigStr, DeidMaskingConfig.class);

    assertEquals(3, maskingConfig.getRules().size());
    assertEquals(858, maskingConfig.getJson().getMaskingRules().size());
  }
}
