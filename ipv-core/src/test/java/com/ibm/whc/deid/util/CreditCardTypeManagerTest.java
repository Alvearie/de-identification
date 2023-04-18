/*
 * © Merative US L.P. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import org.junit.Test;
import com.ibm.whc.deid.models.CreditCardType;
import com.ibm.whc.deid.providers.masking.MaskingProviderTest;
import com.ibm.whc.deid.shared.exception.KeyedRuntimeException;
import com.ibm.whc.deid.utils.log.LogCodes;

public class CreditCardTypeManagerTest implements MaskingProviderTest {

  @Test
  public void testBadInput() throws Exception {
    try {
      CreditCardTypeManager.buildCreditCardTypeManager(ERROR_LOCALIZATION_PROPERTIES);
      fail("expected exception");
    } catch (KeyedRuntimeException e) {
      assertEquals(LogCodes.WPH1023E, e.getMessageKey());
      assertEquals(
          "Invalid values were encountered while reading record CSVRecord [comment='null', recordNumber=7, values=[Diners Club, 300:301: :303:304:305:36:38, 14, 14]] from /localization/test.credit_card_types.bad.csv: The value \"300:301: :303:304:305:36:38\" for \"card prefixes\" is invalid",
          e.getMessage());
    }
  }

  @Test
  public void testLoadRecord() {
    String[] record =
        new String[] {"MyCard", "6011:6022", "10", "10"};
    CreditCardTypeManager manager = new CreditCardTypeManager();
    String locale = "en";

    CreditCardTypeManager.loadRecord(locale, manager, record);
    CreditCardType resource = manager.getValue("myCARD");
    assertNotNull(resource);
    assertEquals("MyCard", resource.getName());
    String[] prefixes = resource.getPrefixes();
    assertEquals(2, prefixes.length);
    assertEquals("6011", prefixes[0]);
    assertEquals("6022", prefixes[1]);
    assertEquals(10, resource.getMinimumLength());
    assertEquals(10, resource.getMaximumLength());
    assertSame(resource, manager.getValue("eN", "myCaRD"));

    record[3] = "12";
    // bad name
    String temp = record[0];
    record[0] = null;
    try {
      CreditCardTypeManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("card type name"));
      assertTrue(e.getMessage().contains("null"));
    }
    record[0] = " ";
    try {
      CreditCardTypeManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("card type name"));
    }
    record[0] = temp;

    // bad prefixes
    temp = record[1];
    record[1] = null;
    try {
      CreditCardTypeManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("card prefixes"));
      assertTrue(e.getMessage().contains("null"));
    }
    record[1] = "  ";
    try {
      CreditCardTypeManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("card prefixes"));
    }
    record[1] = ":";
    try {
      CreditCardTypeManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("card prefixes"));
    }
    record[1] = ":88";
    try {
      CreditCardTypeManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("card prefixes"));
      assertTrue(e.getMessage().contains(":88"));
    }
    // note trailing : allowed by parser
    record[1] = "8: :88";
    try {
      CreditCardTypeManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("card prefixes"));
      assertTrue(e.getMessage().contains("8: :88"));
    }
    record[1] = temp;

    // bad min length
    temp = record[2];
    record[2] = null;
    try {
      CreditCardTypeManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("card minimum length"));
      assertTrue(e.getMessage().contains("null"));
    }
    record[2] = "   ";
    try {
      CreditCardTypeManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("card minimum length"));
    }
    record[2] = "axe";
    try {
      CreditCardTypeManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("card minimum length"));
      assertTrue(e.getMessage().contains("axe"));
    }
    record[2] = "0";
    try {
      CreditCardTypeManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("card minimum length"));
      assertTrue(e.getMessage().contains("0"));
    }
    record[2] = temp;

    // bad max length
    temp = record[3];
    record[3] = null;
    try {
      CreditCardTypeManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("card maximum length"));
      assertTrue(e.getMessage().contains("null"));
    }
    record[3] = "   ";
    try {
      CreditCardTypeManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("card maximum length"));
    }
    record[3] = "axe";
    try {
      CreditCardTypeManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("card maximum length"));
      assertTrue(e.getMessage().contains("axe"));
    }
    record[3] = "9";
    try {
      CreditCardTypeManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("card maximum length"));
      assertTrue(e.getMessage().contains("9"));
    }
    record[3] = temp;
  }
}
