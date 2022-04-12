/*
 * (C) Copyright IBM Corp. 2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fhir;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import java.time.LocalDate;
import org.junit.Test;
import com.ibm.whc.deid.shared.pojo.config.masking.FHIRMortalityDependencyMaskingProviderConfig;


public class FHIRMortalityDependencyMaskingProviderTest {

  // ------------------------------------------------------------------------------------------------------------------------------
  // See also com.ibm.whc.deid.providers.masking.fhir.FHIRMortalityDependencyMaskingProviderEndToEndTest in de-identification-app.
  // ------------------------------------------------------------------------------------------------------------------------------

  @Test
  public void testGetDateFromString() {
    FHIRMortalityDependencyMaskingProvider provider = new FHIRMortalityDependencyMaskingProvider(
        new FHIRMortalityDependencyMaskingProviderConfig());

    assertEquals(LocalDate.of(2002, 2, 1), provider.getDateFromString("2002-02-01", "prop1"));

    LocalDate date = provider.getDateFromString("0002-02-01", "prop1");
    assertEquals(LocalDate.of(2, 2, 1), date);
    assertEquals("0002-02-01", date.toString());

    date = provider.getDateFromString("0022-02-01", "prop1");
    assertEquals(LocalDate.of(22, 2, 1), date);
    assertEquals("0022-02-01", date.toString());

    date = provider.getDateFromString("0222-02-01", "prop1");
    assertEquals(LocalDate.of(222, 2, 1), date);
    assertEquals("0222-02-01", date.toString());

    date = provider.getDateFromString("2013-12-18Tblah", "prop1");
    assertEquals(LocalDate.of(2013, 12, 18), date);
    assertEquals("2013-12-18", date.toString());

    date = provider.getDateFromString("2013-01", "prop1");
    assertEquals(LocalDate.of(2013, 1, 1), date);
    assertEquals("2013-01-01", date.toString());

    date = provider.getDateFromString("2013-04", "prop1");
    assertEquals(LocalDate.of(2013, 4, 1), date);
    assertEquals("2013-04-01", date.toString());

    date = provider.getDateFromString("2013-12", "prop1");
    assertEquals(LocalDate.of(2013, 12, 1), date);
    assertEquals("2013-12-01", date.toString());

    assertNull(provider.getDateFromString("222-02-01", "prop1"));
    assertNull(provider.getDateFromString("22-02-01", "prop1"));
    assertNull(provider.getDateFromString("2-02-01", "prop1"));
    assertNull(provider.getDateFromString("2222-2-01", "prop1"));
    assertNull(provider.getDateFromString("2222-02-1", "prop1"));
    assertNull(provider.getDateFromString("3", "prop1"));
    assertNull(provider.getDateFromString("33", "prop1"));
    assertNull(provider.getDateFromString("333", "prop1"));
    assertNull(provider.getDateFromString("3-", "prop1"));
    assertNull(provider.getDateFromString("33-", "prop1"));
    assertNull(provider.getDateFromString("333-", "prop1"));
    assertNull(provider.getDateFromString("3-01", "prop1"));
    assertNull(provider.getDateFromString("33-01", "prop1"));
    assertNull(provider.getDateFromString("333-01", "prop1"));
    assertNull(provider.getDateFromString("1111-4", "prop1"));
    assertNull(provider.getDateFromString("1111-13", "prop1"));
    assertNull(provider.getDateFromString("1111-00", "prop1"));
    assertNull(provider.getDateFromString("11111-00", "prop1"));
    assertNull(provider.getDateFromString("02022-01", "prop1"));
    assertNull(provider.getDateFromString("11111-001", "prop1"));
    assertNull(provider.getDateFromString("2013-01-03-08", "prop1"));
    assertNull(provider.getDateFromString("abcd-01", "prop1"));
    assertNull(provider.getDateFromString("1234-xb", "prop1"));
    assertNull(provider.getDateFromString("201x-01-03", "prop1"));
    assertNull(provider.getDateFromString("2014/01/03", "prop1"));
    assertNull(provider.getDateFromString("03/01/2014", "prop1"));
    assertNull(provider.getDateFromString("2014/01/13", "prop1"));
    assertNull(provider.getDateFromString("2014/13/01", "prop1"));
    assertNull(provider.getDateFromString("2014/11/31", "prop1"));
    assertNull(provider.getDateFromString("2013-00-12", "prop1"));
    assertNull(provider.getDateFromString("2013-13-12", "prop1"));
    assertNull(provider.getDateFromString("2013-11-00", "prop1"));
    assertNull(provider.getDateFromString("2013-12-32", "prop1"));
    assertNull(provider.getDateFromString("2013-04-31", "prop1"));
    assertNull(provider.getDateFromString("2013-02-30", "prop1"));
    assertNull(provider.getDateFromString("2013-02-29", "prop1"));
    assertNull(provider.getDateFromString("", "prop1"));
    assertNull(provider.getDateFromString("    -  -  ", "prop1"));
  }
}
