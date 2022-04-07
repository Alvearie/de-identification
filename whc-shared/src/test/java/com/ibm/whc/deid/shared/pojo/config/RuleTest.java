/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import java.util.ArrayList;
import java.util.List;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ibm.whc.deid.shared.pojo.config.masking.CityMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.MaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.RedactMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderType;

public class RuleTest {

  private static final Logger logger = LoggerFactory.getLogger(RuleTest.class);

  @Test
  public void testSerializeRule() throws Exception {
    List<MaskingProviderConfig> maskingProviders = new ArrayList<>();

    Rule rule = new Rule("rule1", maskingProviders);

    CityMaskingProviderConfig city = new CityMaskingProviderConfig();
    city.setMaskClosestK(2);

    maskingProviders.add(city);

    RedactMaskingProviderConfig redact = new RedactMaskingProviderConfig();
    maskingProviders.add(redact);

    ObjectMapper objectMapper = new ObjectMapper();

    String json = objectMapper.writeValueAsString(rule);

    logger.info(json);

    assertNotNull(json);
  }

  @Test
  public void testDeserialize() throws Exception {
    String json =
        "{\"maskingProviders\":[{\"type\":\"CITY\",\"maskClosest\":false,\"maskPseudorandom\":false},{\"type\":\"REDACT\",\"preserveLength\":false}]}";

    ObjectMapper objectMapper = new ObjectMapper();
    Rule rule = objectMapper.readValue(json, Rule.class);

    CityMaskingProviderConfig cityConfig =
        (CityMaskingProviderConfig) rule.getMaskingProviders().get(0);
    assertEquals(MaskingProviderType.CITY, cityConfig.getType());

    assertTrue(rule.getMaskingProviders().get(0) instanceof CityMaskingProviderConfig);
    assertTrue(rule.getMaskingProviders().get(1) instanceof RedactMaskingProviderConfig);
  }

  @Test
  public void testDeserializeMissingFields() throws Exception {
    String json =
        "{\"maskingProviders\":[{\"type\":\"CITY\",\"maskClosest\":false,\"maskClosestK\":2,\"maskPseudorandom\":false},{\"type\":\"REDACT\"}]}";

    ObjectMapper objectMapper = new ObjectMapper();
    Rule rule = objectMapper.readValue(json, Rule.class);

    assertTrue(rule.getMaskingProviders().get(0) instanceof CityMaskingProviderConfig);
    assertTrue(rule.getMaskingProviders().get(1) instanceof RedactMaskingProviderConfig);
  }
}
