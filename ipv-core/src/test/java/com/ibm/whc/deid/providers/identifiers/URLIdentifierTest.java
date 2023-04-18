/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class URLIdentifierTest {
  @Test
  public void testIsOfThisType() throws Exception {

    URLIdentifier identifier = new URLIdentifier();

    String[] validURLs = {"http://www.nba.com", "http://www.nba.co.uk", "https://www.nba.com",
        "http://www.nba.com/index.html", "http://www.nba.com/index.html?q=MichaelJordan",
        "http://www.nba.com:8080", "http://22.33.44.55", "http://22.33.44.55:8080",
        "https://22.33.44.55:8080", "http://username@test.com", "https://username@test.com",
        "http://username:password@test.com", "https://username:password@test.com",
        "http://[2001:db8:1f70::999:de8:7648:6e8]/index.html",
        "http://[2001:db8:1f70::999:de8:7648:6e8]:100/"};

    for (String validURL : validURLs) {
      assertTrue(identifier.isOfThisType(validURL));
    }
  }
}
