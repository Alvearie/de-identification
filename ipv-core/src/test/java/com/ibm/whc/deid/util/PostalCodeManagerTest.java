/*
 * © Merative US L.P. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.util.List;
import org.junit.Ignore;
import org.junit.Test;
import com.ibm.whc.deid.models.LatitudeLongitude;
import com.ibm.whc.deid.models.PostalCode;
import com.ibm.whc.deid.providers.masking.MaskingProviderTest;
import com.ibm.whc.deid.shared.exception.KeyedRuntimeException;
import com.ibm.whc.deid.utils.log.LogCodes;


public class PostalCodeManagerTest implements MaskingProviderTest {

  @Test
  public void testBadInput() throws Exception {
    try {
      PostalCodeManager.buildPostalCodeManager(ERROR_LOCALIZATION_PROPERTIES);
      fail("expected exception");
    } catch (KeyedRuntimeException e) {
      assertEquals(LogCodes.WPH1023E, e.getMessageKey());
      assertEquals(
          "Invalid values were encountered while reading record CSVRecord [comment='null', recordNumber=5, values=[US, 99583, 54.841, -183.4368]] from /localization/test.postal_codes.bad.csv: The value \"-183.4368\" for \"longitude\" is invalid",
          e.getMessage());
    }
  }

  @Test
  public void testLoadRecord() {
    PostalCodeManager manager = new PostalCodeManager();

    String[] record = new String[] {null, "58046", null, null};

    PostalCodeManager.loadRecord(manager, record);
    PostalCode postal = manager.getValue("58046");
    assertNotNull(postal);
    assertEquals("58046", postal.getName());
    assertNull(postal.getLocation());
    List<PostalCode> nearest = manager.getClosestPostalCodes(postal.getName(), 5);
    assertNotNull(nearest);
    assertEquals(0, nearest.size());

    record = new String[] {"", "55902", "23.4", "-78.3"};

    PostalCodeManager.loadRecord(manager, record);
    postal = manager.getValue("55902");
    assertNotNull(postal);
    assertEquals("55902", postal.getName());
    LatitudeLongitude location = postal.getLocation();
    assertNotNull(location);
    assertEquals(23.4, location.getLatitude(), 0);
    assertEquals(-78.3, location.getLongitude(), 0);
    nearest = manager.getClosestPostalCodes(postal.getName(), 5);
    assertNotNull(nearest);
    assertEquals(0, nearest.size());

    record = new String[] {"x", "55901", "23.3", "-78.3"};

    PostalCodeManager.loadRecord(manager, record);
    postal = manager.getValue("55901");
    assertNotNull(postal);
    assertEquals("55901", postal.getName());
    location = postal.getLocation();
    assertNotNull(location);
    assertEquals(23.3, location.getLatitude(), 0);
    assertEquals(-78.3, location.getLongitude(), 0);
    nearest = manager.getClosestPostalCodes(postal.getName(), 5);
    assertNotNull(nearest);
    assertEquals(1, nearest.size());
    assertEquals("55902", nearest.get(0).getName());

    // bad name
    String temp = record[1];
    record[1] = null;
    try {
      PostalCodeManager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("postal code"));
    }
    record[1] = " ";
    try {
      PostalCodeManager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("postal code"));
    }
    record[1] = temp;

    // bad latitude
    temp = record[2];
    record[2] = "AX";
    try {
      PostalCodeManager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("latitude"));
    }
    record[2] = "-91.4";
    try {
      PostalCodeManager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("latitude"));
    }
    record[2] = "91.4";
    try {
      PostalCodeManager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("latitude"));
    }
    record[2] = temp;

    // bad longitude
    temp = record[3];
    record[3] = "AX";
    try {
      PostalCodeManager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("longitude"));
    }
    record[3] = "-181.4";
    try {
      PostalCodeManager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("longitude"));
    }
    record[3] = "191.4";
    try {
      PostalCodeManager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("longitude"));
    }
    record[3] = temp;
  }

  @Test
  public void testLookup() throws Exception {
    PostalCodeManager postalCodeManager =
        PostalCodeManager.buildPostalCodeManager(localizationProperty);
    String code = "99503";
    assertTrue(postalCodeManager.isValidKey(code));
    PostalCode postalCode = postalCodeManager.getValue(code);
    assertNotNull(postalCode);
    assertEquals(code, postalCode.getName());
    assertTrue(61.19 == postalCode.getLocation().getLatitude());
    assertTrue(-149.8938 == postalCode.getLocation().getLongitude());

    code = "xx";
    assertFalse(postalCodeManager.isValidKey(code));
    assertNull(postalCodeManager.getValue(code));

    code = null;
    assertFalse(postalCodeManager.isValidKey(code));
    assertNull(postalCodeManager.getValue(code));
  }

  @Test
  public void testRandomCodeGenerator() throws Exception {
    PostalCodeManager postalCodeManager =
        PostalCodeManager.buildPostalCodeManager(localizationProperty);
    assertTrue(postalCodeManager.isValidKey(postalCodeManager.getRandomKey()));
  }

  @Test
  @Ignore
  public void testPerformance() {
    PostalCodeManager postalCodeManager =
        PostalCodeManager.buildPostalCodeManager(localizationProperty);
    int N = 1000000;
    long startMillis = System.currentTimeMillis();
    String code = "99503";

    for (int i = 0; i < N; i++) {
      postalCodeManager.getClosestPostalCodes(code, 10);
    }

    long diff = System.currentTimeMillis() - startMillis;
    System.out.println(String.format("%d operations took %d milliseconds (%f per op)", N, diff,
        (double) diff / N));
    // Assert test always should finish in less than 10 seconds
    assertTrue(diff < 10000);
  }

  @Test
  public void testClosestCode() throws Exception {
    PostalCodeManager postalCodeManager =
        PostalCodeManager.buildPostalCodeManager(localizationProperty);
    String originalCode = "55901";

    List<PostalCode> nearest = postalCodeManager.getClosestPostalCodes(originalCode, 5);
    assertNotNull(nearest);
    assertEquals(5, nearest.size());
    assertEquals("55905", nearest.get(0).getName());
    assertEquals("55902", nearest.get(1).getName());
    assertEquals("55903", nearest.get(2).getName());
    assertEquals("55960", nearest.get(3).getName());
    assertEquals("55904", nearest.get(4).getName());

    nearest = postalCodeManager.getClosestPostalCodes("00000", 10);
    assertNotNull(nearest);
    assertEquals(0, nearest.size());
  }
}
