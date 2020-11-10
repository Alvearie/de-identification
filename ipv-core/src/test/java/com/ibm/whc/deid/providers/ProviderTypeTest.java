/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import org.junit.Test;
import com.fasterxml.jackson.databind.ObjectMapper;

public class ProviderTypeTest {
  @Test
  public void testFriendlyName() {
    String friendlyName = ProviderType.CITY.getFriendlyName();
    assertTrue(friendlyName.equals("City"));
  }

  @Test
  public void serialization() throws Exception {
    ObjectMapper mapper = new ObjectMapper();

    for (ProviderType type : ProviderType.publicValues()) {
      String s = mapper.writeValueAsString(type);
      System.out.println(s);
      assertNotNull(s);
    }
  }
}
