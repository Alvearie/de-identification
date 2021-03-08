/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ibm.whc.deid.shared.pojo.config.masking.HashMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.RedactMaskingProviderConfig;
import com.ibm.whc.deid.shared.util.ConfigGenerator;

public class DeidMaskingConfigTest {

  private static final Logger log = LoggerFactory.getLogger(DeidMaskingConfigTest.class);

  @Test
  public void testRules() {
    DeidMaskingConfig config = new DeidMaskingConfig();
    assertNull(config.getRules());
    assertNotNull(config.getRulesMap());
    assertEquals(0, config.getRulesMap().size());
    config.setRules(new ArrayList<Rule>());
    assertEquals(0, config.getRulesMap().size());

    List<Rule> rules = new ArrayList<>();
    rules.add(new Rule("rule1",
        Arrays.asList(new HashMaskingProviderConfig(), new RedactMaskingProviderConfig())));
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
