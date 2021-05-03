/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import org.junit.Test;
import com.ibm.whc.deid.providers.masking.MaskingProviderTest;

public class SWIFTCodeIdentifierTest implements MaskingProviderTest {

  @Test
  public void testCodesLoaded() {
    SWIFTCodeIdentifier id = new SWIFTCodeIdentifier(tenantId, TEST_LOCALIZATION_PROPERTIES);
    assertFalse(id.isOfThisType("ABC"));
    assertFalse(id.isOfThisType("ABCDEFGH"));
    assertTrue(id.isOfThisType("HHHHUSHH"));
    assertTrue(id.isOfThisType("ddddCADD004"));
    assertFalse(id.isOfThisType(null));
    assertFalse(id.isOfThisType(""));
  }

  @Test
  public void testNoCodesLoaded() {
    SWIFTCodeIdentifier id = new SWIFTCodeIdentifier(tenantId, localizationProperty);
    assertFalse(id.isOfThisType("ABC"));
    assertTrue(id.isOfThisType("ABCDEFGH"));
    assertTrue(id.isOfThisType("HHHHUSHH"));
    assertTrue(id.isOfThisType("ddddCAdd"));
    assertFalse(id.isOfThisType("dddd11dd"));
    assertTrue(id.isOfThisType("HHhhUS99abc"));
    assertTrue(id.isOfThisType("HHhhUSxx888"));
    assertFalse(id.isOfThisType("HHhhUS99ab"));
    assertFalse(id.isOfThisType("HHhhUS99a"));
    assertTrue(id.isOfThisType("ddddCAdd"));
    assertFalse(id.isOfThisType(null));
    assertFalse(id.isOfThisType(""));
  }
}
