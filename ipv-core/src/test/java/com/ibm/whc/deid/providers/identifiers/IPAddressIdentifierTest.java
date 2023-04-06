/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class IPAddressIdentifierTest {

  @Test
  public void testIsOfThisType() throws Exception {
    IPAddressIdentifier identifier = new IPAddressIdentifier();

    assertTrue(identifier.isOfThisType("1.2.3.4"));

    // letters are not allowed
    assertFalse(identifier.isOfThisType("a.b.0.1"));

    // each prefix of IP address should be <= 255
    assertFalse(identifier.isOfThisType("1111.2.3.4"));

    // wrong format
    assertFalse(identifier.isOfThisType(".2.3.4"));

    // valid format
    assertTrue(identifier.isOfThisType("::"));

    String[] validIPv6Addresses = {"1:2:3:4:5:6:7:8", "1::", "1::8", "1::7:8", "1::6:7:8",
        "1::5:6:7:8", "1::4:5:6:7:8", "1::3:4:5:6:7:8", "fe80::7:8%eth0", "::255.255.255.255",
        "::ffff:255.255.255.255", "::FFFF:255.255.255.255", "::ffff:0:255.255.255.255",
        "::FFFF:0:255.255.255.255", "2001:db8:3:4::192.0.2.33", "64:ff9b::192.0.2.33"};

    for (String ipv6address : validIPv6Addresses) {
      // System.out.println(ipv6address);
      assertTrue(identifier.isOfThisType(ipv6address));
    }
  }
}
