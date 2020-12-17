/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fhir;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
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

  @Test
  public void testMainConstructor() throws Exception {
    ArrayList<FHIRResourceField> fields = null;
    FHIRResourceMaskingConfiguration config =
        new FHIRResourceMaskingConfiguration("/fhir/Device", fields);
    assertEquals("/fhir/Device", config.getBasePath());
    assertNotNull(config.getFields());
    assertEquals(0, config.getFields().size());

    fields = new ArrayList<>();
    config = new FHIRResourceMaskingConfiguration("/fhir/Patient", fields);
    assertEquals("/fhir/Patient", config.getBasePath());
    assertNotNull(config.getFields());
    assertEquals(0, config.getFields().size());

    fields.add(new FHIRResourceField("/a/b/c", "redact"));
    config = new FHIRResourceMaskingConfiguration("/fhir/Device", fields);
    assertEquals("/fhir/Device", config.getBasePath());
    assertNotNull(config.getFields());
    Iterator<FHIRResourceField> it = config.getFields().iterator();
    assertTrue(it.hasNext());
    FHIRResourceField field = it.next();
    assertEquals("/a/b/c", field.getKey());
    assertEquals("redact", field.getShortRuleName());
    assertFalse(it.hasNext());

    fields.add(new FHIRResourceField("/a/b/C", "redact2"));
    fields.add(new FHIRResourceField("/a/b/c", "redact3x"));
    fields.add(new FHIRResourceField("/a/b/c/d", "hash"));
    fields.add(new FHIRResourceField("/a/b/c/e[3]", "e3x"));
    fields.add(new FHIRResourceField("/a/b/c/e[4]", "e4"));
    fields.add(new FHIRResourceField("/a/b/c/f(System==phone)", "phoneX"));
    fields.add(new FHIRResourceField("/a/b/c/e[3]", "e3"));
    fields.add(new FHIRResourceField("/a/b/c", "redact3"));
    fields.add(new FHIRResourceField("/a/b/c/f(System==phone)", "phone"));
    config = new FHIRResourceMaskingConfiguration("/fhir/Observation", fields);
    assertEquals("/fhir/Observation", config.getBasePath());
    assertNotNull(config.getFields());
    it = config.getFields().iterator();
    assertTrue(it.hasNext());
    field = it.next();
    assertEquals("/a/b/C", field.getKey());
    assertEquals("redact2", field.getShortRuleName());
    assertTrue(it.hasNext());
    field = it.next();
    assertEquals("/a/b/c/d", field.getKey());
    assertEquals("hash", field.getShortRuleName());
    assertTrue(it.hasNext());
    field = it.next();
    assertEquals("/a/b/c/e[4]", field.getKey());
    assertEquals("e4", field.getShortRuleName());
    assertTrue(it.hasNext());
    field = it.next();
    assertEquals("/a/b/c/e[3]", field.getKey());
    assertEquals("e3", field.getShortRuleName());
    assertTrue(it.hasNext());
    field = it.next();
    assertEquals("/a/b/c", field.getKey());
    assertEquals("redact3", field.getShortRuleName());
    assertTrue(it.hasNext());
    field = it.next();
    assertEquals("/a/b/c/f(System==phone)", field.getKey());
    assertEquals("phone", field.getShortRuleName());
    assertFalse(it.hasNext());
  }

}
