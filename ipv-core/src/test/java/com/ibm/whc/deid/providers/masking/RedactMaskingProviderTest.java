/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import org.junit.Test;
import com.ibm.whc.deid.shared.pojo.config.masking.RedactMaskingProviderConfig;

public class RedactMaskingProviderTest extends TestLogSetUp {

  /*
   * Notes: Tests for preserve length boolean option (true and false), and also the replace
   * character option.
   */
  @Test
  public void testMask() {
    // By default preserve length is true and replace character is "X"

    RedactMaskingProvider maskingProvider = new RedactMaskingProvider();
    String original = "redact ,d99";

    String mask = maskingProvider.mask(original);
    // tests length and replacement character
    assertEquals(mask, "XXXXXXXXXXX");
  }

  @Test
  public void testNoPreserveLength() {
    RedactMaskingProviderConfig configuration = new RedactMaskingProviderConfig();
    configuration.setReplaceCharacter("X");
    configuration.setPreserveLength(false);

    RedactMaskingProvider maskingProvider = new RedactMaskingProvider(configuration);
    String original = "redact-this";

    String mask = maskingProvider.mask(original);
    assertEquals(mask, "X");
  }

  @Test
  public void testMaskNullRedactInputReturnNull() throws Exception {
    RedactMaskingProviderConfig configuration = new RedactMaskingProviderConfig();

    RedactMaskingProvider maskingProvider = new RedactMaskingProvider(configuration);

    String invalidRedact = null;
    String maskedRedact = maskingProvider.mask(invalidRedact);

    assertEquals(null, maskedRedact);
    assertTrue(outContent.toString().contains("DEBUG - WPH1015D"));
  }
}
