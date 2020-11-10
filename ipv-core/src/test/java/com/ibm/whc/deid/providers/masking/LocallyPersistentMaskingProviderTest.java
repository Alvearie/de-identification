/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class LocallyPersistentMaskingProviderTest extends TestLogSetUp {
  @Test
  public void testPersistence() {

    String email1 = "joedoe1@foo.com";
    String email2 = "joedoe2@foo.com";

    MaskingProvider emailMaskingProvider = new EmailMaskingProvider();
    LocallyPersistentMaskingProvider locallyPersistentMaskingProvider =
        new LocallyPersistentMaskingProvider(emailMaskingProvider);

    String maskedEmail1_once = locallyPersistentMaskingProvider.mask(email1);
    String maskedEmail1_twice = locallyPersistentMaskingProvider.mask(email1);
    assertTrue(maskedEmail1_once.equals(maskedEmail1_twice));

    String maskedEmail2_once = locallyPersistentMaskingProvider.mask(email2);
    assertFalse(maskedEmail2_once.equals(maskedEmail1_once));
  }
}
