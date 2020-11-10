/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fhir;

import static org.junit.Assert.assertEquals;
import java.util.HashMap;
import java.util.Map;
import org.junit.Test;

public class FHIRResourceMaskingConfigurationTest {
  @Test
  public void testParsing() throws Exception {
    Map<String, String> deviceMaskConf = new HashMap<>();
    deviceMaskConf.put("/fhir/Device/owner", "/owner");

    FHIRResourceMaskingConfiguration resourceConfiguration =
        new FHIRResourceMaskingConfiguration("/fhir/Device", deviceMaskConf);

    assertEquals("/fhir/Device", resourceConfiguration.getBasePath());
    assertEquals(1, resourceConfiguration.getFields().size());
    assertEquals("/owner", resourceConfiguration.getFields().get(0).getShortRuleName());
  }

}
