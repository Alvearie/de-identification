/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.junit.Test;
import com.ibm.whc.deid.providers.identifiers.StatesUSIdentifier;
import com.ibm.whc.deid.shared.pojo.config.masking.StatesUSMaskingProviderConfig;

public class StatesUSMaskingProviderTest {

  @Test
  public void testMask() {
    String tenantId = null;
    StatesUSIdentifier statesUSIdentifier = new StatesUSIdentifier();
    StatesUSMaskingProviderConfig maskingConfiguration = new StatesUSMaskingProviderConfig();
    StatesUSMaskingProvider maskingProvider =
        new StatesUSMaskingProvider(maskingConfiguration, tenantId);
    assertNull(maskingProvider.statesUSManager);

    String maskedValue = maskingProvider.mask("Minnesota");
    assertTrue(statesUSIdentifier.isOfThisType(maskedValue));
    assertTrue(maskedValue.length() > 2);
    assertNotEquals(maskedValue.toUpperCase(), maskedValue);
    assertNotNull(maskingProvider.statesUSManager);
    int hashcode = System.identityHashCode(maskingProvider.statesUSManager);

    String[] names = new String[] {"alaska", "hawaii", "washington", "oregon", "california",
        "idaho", "montana", "wyoming", "utah", "nevada", "colorado", "arizona", "new mexico",
        "north dakota", "south dakota", "nebraska", "kansas", "oklahoma", "texas", "minnesota",
        "iowa", "missouri", "arkansas", "louisiana", "wisconsin", "michigan", "illinois", "indiana",
        "kentucky", "tennessee", "virginia", "north carolina", "south carolina", "georgia",
        "florida", "alabama", "mississippi", "ohio", "west virginia", "pennsylvania", "new york",
        "new jersey", "delaware", "maryland", "maine", "vermont", "new hampshire", "massachusetts",
        "rhode island", "connecticut"};
    for (String name : names) {
      maskedValue = maskingProvider.mask(name);
      assertNotNull("null produced for: " + name, maskedValue);
      assertTrue(statesUSIdentifier.isOfThisType(maskedValue));
      assertTrue(maskedValue.length() > 2);
      assertNotEquals(maskedValue.toUpperCase(), maskedValue);

      name = name.toUpperCase();
      maskedValue = maskingProvider.mask(name);
      assertNotNull("null produced for: " + name, maskedValue);
      assertTrue(statesUSIdentifier.isOfThisType(maskedValue));
      assertTrue(maskedValue.length() > 2);
      assertNotEquals(maskedValue.toUpperCase(), maskedValue);
    }

    String[] abbrevs = new String[] {"AK", "HI", "WA", "OR", "CA", "ID", "MT", "WY", "UT", "NV",
        "CO", "AZ", "NM", "ND", "SD", "NE", "KS", "OK", "TX", "MN", "IA", "MO", "AR", "LA", "WI",
        "MI", "IL", "IN", "KY", "TN", "VA", "NC", "SC", "GA", "FL", "AL", "MS", "OH", "WV", "PA",
        "NY", "NJ", "DE", "MD", "ME", "VT", "NH", "MA", "RI", "CT"};
    for (String abbrev : abbrevs) {
      maskedValue = maskingProvider.mask(abbrev);
      assertNotNull("null produced for: " + abbrev, maskedValue);
      assertTrue("unrecognized value produced: " + maskedValue + " for: " + abbrev,
          statesUSIdentifier.isOfThisType(maskedValue));
      assertEquals(2, maskedValue.length());
      assertEquals(maskedValue.toUpperCase(), maskedValue);

      String lc = abbrev.toLowerCase();
      maskedValue = maskingProvider.mask(lc);
      assertNotNull("null produced for: " + lc, maskedValue);
      assertTrue("unrecognized value produced: " + maskedValue + " for: " + lc,
          statesUSIdentifier.isOfThisType(maskedValue));
      assertEquals(2, maskedValue.length());
      assertEquals(maskedValue.toUpperCase(), maskedValue);
    }

    maskedValue = maskingProvider.mask("minnESOta");
    assertTrue(statesUSIdentifier.isOfThisType(maskedValue));
    assertTrue(maskedValue.length() > 2);
    assertNotEquals(maskedValue.toUpperCase(), maskedValue);

    assertNull(maskingProvider.mask((String) null));
    assertNull(maskingProvider.mask(""));
    assertNull(maskingProvider.mask("notState"));
    assertNull(maskingProvider.mask("IU"));
    assertNull(maskingProvider.mask("minitoba"));

    assertEquals(hashcode, System.identityHashCode(maskingProvider.statesUSManager));

    maskingConfiguration.setUnspecifiedValueHandling(2);
    maskingProvider = new StatesUSMaskingProvider(maskingConfiguration, tenantId);
    assertNull(maskingProvider.statesUSManager);

    assertNull(maskingProvider.mask((String) null));
    assertNotNull(maskingProvider.statesUSManager);
    hashcode = System.identityHashCode(maskingProvider.statesUSManager);

    maskedValue = maskingProvider.mask("");
    assertTrue(statesUSIdentifier.isOfThisType(maskedValue));
    assertTrue(maskedValue.length() > 2);
    assertNotEquals(maskedValue.toUpperCase(), maskedValue);

    maskedValue = maskingProvider.mask("not");
    assertTrue(statesUSIdentifier.isOfThisType(maskedValue));
    assertTrue(maskedValue.length() > 2);
    assertNotEquals(maskedValue.toUpperCase(), maskedValue);

    maskedValue = maskingProvider.mask("XX");
    assertTrue(statesUSIdentifier.isOfThisType(maskedValue));
    assertTrue(maskedValue.length() > 2);
    assertNotEquals(maskedValue.toUpperCase(), maskedValue);

    assertEquals(hashcode, System.identityHashCode(maskingProvider.statesUSManager));

    maskingConfiguration.setUnspecifiedValueHandling(3);
    maskingConfiguration.setUnspecifiedValueReturnMessage("unknown");
    maskingProvider = new StatesUSMaskingProvider(maskingConfiguration, tenantId);
    assertNull(maskingProvider.statesUSManager);

    maskedValue = maskingProvider.mask("");
    assertEquals("unknown", maskedValue);
    assertNotNull(maskingProvider.statesUSManager);
    hashcode = System.identityHashCode(maskingProvider.statesUSManager);

    maskedValue = maskingProvider.mask("not");
    assertEquals("unknown", maskedValue);

    maskedValue = maskingProvider.mask("XX");
    assertEquals("unknown", maskedValue);

    assertNull(maskingProvider.mask((String) null));

    assertEquals(hashcode, System.identityHashCode(maskingProvider.statesUSManager));
  }
}
