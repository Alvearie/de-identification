/*
 * © Merative US L.P. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fpe;

import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class EncryptionEngineExceptionTest {

  @Test
  public void testMain() {
    EncryptionEngineException e =
        new EncryptionEngineException(new IllegalArgumentException("do not use"));
    assertEquals("IPV-core error : \"encryption engine failure\"", e.getMessage());
  }
}
