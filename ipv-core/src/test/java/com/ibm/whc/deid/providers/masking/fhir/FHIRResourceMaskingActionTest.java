/*
 * © Merative US L.P. 2016,2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fhir;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class FHIRResourceMaskingActionTest {
  @Test
  public void testPaths() {
    FHIRResourceMaskingAction maskingAction =
        new FHIRResourceMaskingAction("/fhir/Device/owner/details", "/owner/details", null);

    String[] paths = maskingAction.getPaths();
    assertEquals(2, paths.length);
    assertEquals("owner", paths[0]);
    assertEquals("details", paths[1]);
  }
}
