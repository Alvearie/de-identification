/*
 * © Merative US L.P. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fpe;

import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class UnsupportedLengthExceptionTest {

  @Test
  public void testMain() {
    UnsupportedLengthException e = new UnsupportedLengthException(1, 2, 3);
    assertEquals("Processing requires 2-3 input characters, but 1 were available.", e.getMessage());
    assertEquals(1, e.getLength());
    assertEquals(2, e.getMin());
    assertEquals(3, e.getMax());
  }
}
