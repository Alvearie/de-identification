/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import com.ibm.whc.deid.shared.pojo.config.masking.NullMaskingProviderConfig;
import org.junit.Test;

public class NullMaskingProviderTest {
  /*
   * Notes: Two unit test cases with origin value of null.mask.returnNull" = true and the default
   * value (false) to get back a null or an empty string respectively.
   */
  @Test
  public void testDefault() {
    NullMaskingProvider maskingProvder = new NullMaskingProvider();
    String original = "to_be_emptied_value";

    String mask = maskingProvder.mask(original);
    // System.out.println("=======> " + this.getClass().getName() +
    // " - testDefault(), original [" + original + "], mask[" + mask
    // +"]");
    assertEquals(null, mask);
  }

  @Test
  public void testEmpty() {
    NullMaskingProviderConfig config = new NullMaskingProviderConfig();
    config.setMaskReturnNull(true);

    NullMaskingProvider maskingProvder = new NullMaskingProvider(config);

    String original = "to_be_nulled_value";
    String mask = maskingProvder.mask(original);

    // System.out.println("=======> " + this.getClass().getName() +
    // " - testEmpty(), original [" + original + "], mask[" + mask +"]");
    assertNull(mask);
  }
}
