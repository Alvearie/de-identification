/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import org.junit.Ignore;
import org.junit.Test;
import com.ibm.whc.deid.models.Address;
import com.ibm.whc.deid.models.City;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.CityManager;
import com.ibm.whc.deid.util.ManagerFactory;
import com.ibm.whc.deid.util.localization.LocalizationManager;

public class AddressIdentifierTest {
  @Test
  public void testIsOfThisType() throws Exception {
    AddressIdentifier identifier = new AddressIdentifier();

    /*
     * WIP : (\d+\s){0,1}(?<street>(\w+\s*)+)[St.|Street|Road|Rd.|Avenue|Av.| Drive|
     * Dr.|Boulevard|Blvd.|Court|Ct.]\s*(?<citystate>([A-Za-z]+\s*)*)(?
     * <zip>\d+)\s*(?<country>[A-Za-z]+)*
     */

    String[] validAddresses = {"200 E Main St, Phoenix AZ 85123, USA",
        "200 E Main St., Phoenix AZ 85123, USA", "200 Main Street, Phoenix AZ 85123, USA",
        "200 Main Boulevard, Phoenix AZ 85123, USA", "200 Main Blvd, Phoenix AZ 85123, USA",
        "200 Main Blvd., Phoenix AZ 85123, USA", "200 Main Drive, Phoenix AZ 85123, USA",
        "200 Main Dr., Phoenix AZ 85123, USA", "200 Main Court, Phoenix AZ 85123, USA",
        "200 Main Ct., Phoenix AZ 85123, USA", "300 Bolyston Ave, Seattle WA 98102",
        "300 Bolyston Avenue, Seattle WA 98102", "300 Bolyston Ave., Seattle WA 98102",
        "Hammersmith Bridge Road, London W6 9EJ, United Kingdom",
        "Hammersmith Bridge Road, London W6 9EJ", "20 Rock Road, Blackrock Co. Dublin 15, Ireland",
        "20 Rock Road, Blackrock Co. Dublin 15",
        // "191 E MAIN BOULEVARD, QĀ’EM SHAHR 85241, LTU",
        "2505 SACKETT RUN RD", "1022 WOODLAND AVE", "P.O. BOX 334", "PO BOX 297"};

    for (String address : validAddresses) {
      assertTrue(identifier.isOfThisType(address));
    }
  }

  @Test
  public void testParseAddress() throws Exception {
    String addressName = "200 E Main St, Phoenix AZ 85123, USA";
    AddressIdentifier identifier = new AddressIdentifier();

    Address address = identifier.parseAddress(addressName);
    assertEquals("200", address.getNumber());
    assertTrue(address.getName().equals("E MAIN"));
    assertTrue(address.getRoadType().equals("ST"));
    assertTrue(address.getCityOrState().equals("PHOENIX AZ"));
    assertTrue(address.getPostalCode().equals("85123"));
    assertTrue(address.getCountry().equals("USA"));

    addressName = "Hammersmith Bridge Road, London W6 9EJ";
    address = identifier.parseAddress(addressName);
    assertTrue(address.getNumber().equals(""));
    assertTrue(address.getCountry().equals(""));

    // address without city and country
    addressName = "200 E Main St";
    address = identifier.parseAddress(addressName);
    assertTrue(address.getNumber().equals("200"));
    assertTrue(address.getName().equals("E MAIN"));
    assertTrue(address.getRoadType().equals("ST"));
    assertTrue(address.getCityOrState().equals(""));
    assertTrue(address.getPostalCode().equals(""));
    assertTrue(address.getCountry().equals(""));

    // PO BOX case
    addressName = "PO BOX 777";
    address = identifier.parseAddress(addressName);
    assertTrue(address.isPOBox());
    assertTrue(address.getPoBoxNumber().equals("777"));
  }

  @Test
  public void testParseCity() {
    AddressIdentifier identifier = new AddressIdentifier();
    CityManager cityResourceManager = (CityManager) ManagerFactory.getInstance().getManager("test",
        Resource.CITY, null, LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES);
    StringBuilder buffer = new StringBuilder(100);

    for (City city : cityResourceManager.getValues()) {
      String name = city.getName();
      buffer.setLength(0);
      buffer.append("200 E Main St, ").append(name).append(" 85123, USA");
      String addr = buffer.toString();

      Address address = identifier.parseAddress(addr);
      assertNotNull(address);
      assertEquals("200", address.getNumber());
      assertEquals("E MAIN", address.getName());
      assertEquals("ST", address.getRoadType());
      assertEquals(name.toUpperCase(), address.getCityOrState());
      assertEquals("85123", address.getPostalCode());
      assertEquals("USA", address.getCountry());
    }
  }

  @Test
  public void testParseRoadTypes() throws Exception {
    AddressIdentifier identifier = new AddressIdentifier();
    StringBuilder buffer = new StringBuilder(100);
    buffer.append("200 E Main ");
    int startLen = buffer.length();
    String[] rdtypes = new String[] {"STREET", "ST.", "ST", "DRIVE", "DR.", "DR", "BOULEVARD",
        "BLVD.", "BLVD", "COURT", "CT.", "CT", "ROAD", "RD.", "RD", "AVENUE", "AVE.", "AVE", "LANE",
        "LN.", "LN"};

    String suffix = ", Phoenix AZ 85123, USA";
    for (String rd : rdtypes) {
      buffer.setLength(startLen);
      buffer.append(rd).append(suffix);
      Address address = identifier.parseAddress(buffer.toString());
      assertNotNull(address);
      assertEquals("200", address.getNumber());
      assertEquals("E MAIN", address.getName());
      assertEquals(rd, address.getRoadType());
      assertEquals("PHOENIX AZ", address.getCityOrState());
      assertEquals("85123", address.getPostalCode());
      assertEquals("USA", address.getCountry());
    }

    suffix = "    ";
    for (String rd : rdtypes) {
      buffer.setLength(startLen);
      buffer.append(rd).append(suffix);
      Address address = identifier.parseAddress(buffer.toString());
      assertNotNull(address);
      assertEquals("200", address.getNumber());
      assertEquals("E MAIN", address.getName());
      assertEquals(rd, address.getRoadType());
      assertEquals("", address.getCityOrState());
      assertEquals("", address.getPostalCode());
      assertEquals("", address.getCountry());
    }

    suffix = "";
    for (String rd : rdtypes) {
      buffer.setLength(startLen);
      buffer.append(rd).append(suffix);
      Address address = identifier.parseAddress(buffer.toString());
      assertNotNull(address);
      assertEquals("200", address.getNumber());
      assertEquals("E MAIN", address.getName());
      assertEquals(rd, address.getRoadType());
      assertEquals("", address.getCityOrState());
      assertEquals("", address.getPostalCode());
      assertEquals("", address.getCountry());
    }

    // suffix that includes a road type name after the comma
    suffix = "    , Drive Happy VA 11220";
    for (String rd : rdtypes) {
      buffer.setLength(startLen);
      buffer.append(rd).append(suffix);
      Address address = identifier.parseAddress(buffer.toString());
      assertNotNull(address);
      assertEquals("200", address.getNumber());
      assertEquals("E MAIN", address.getName());
      assertEquals(rd, address.getRoadType());
      assertEquals("DRIVE HAPPY VA", address.getCityOrState());
      assertEquals("11220", address.getPostalCode());
      assertEquals("", address.getCountry());
    }

    // suffix that includes a road type name after the comma but properly delimited with comma
    suffix = ", Drive , CAN";
    for (String rd : rdtypes) {
      buffer.setLength(startLen);
      buffer.append(rd).append(suffix);
      Address address = identifier.parseAddress(buffer.toString());
      assertNotNull(address);
      assertEquals("200", address.getNumber());
      assertEquals("E MAIN", address.getName());
      assertEquals(rd, address.getRoadType());
      assertEquals("DRIVE", address.getCityOrState());
      assertEquals("", address.getPostalCode());
      assertEquals("CAN", address.getCountry());
    }

    // no space after comma and multiple road type names in road type section - i.e. E Main Court St
    suffix = "  ,Hope ND 58046";
    for (String rd : rdtypes) {
      buffer.setLength(startLen);
      buffer.append("court ");
      buffer.append(rd).append(suffix);
      Address address = identifier.parseAddress(buffer.toString());
      assertNotNull(address);
      assertEquals("200", address.getNumber());
      assertEquals("E MAIN COURT", address.getName());
      assertEquals(rd, address.getRoadType());
      assertEquals("HOPE ND", address.getCityOrState());
      assertEquals("58046", address.getPostalCode());
      assertEquals("", address.getCountry());
    }
  }

  @Test
  public void testQA() {
    String value = "The patient gives his own history and appears to be a reliable source.";
    AddressIdentifier identifier = new AddressIdentifier();
    assertFalse(identifier.isOfThisType(value));
  }

  @Test
  @Ignore
  public void testPerformance() {
    int N = 1000000;

    String[] originalValues = new String[] {"200 E Main St, Phoenix AZ 85123, USA", "PO BOX 1234"};

    Identifier identifier = new AddressIdentifier();

    for (String originalValue : originalValues) {
      long startMillis = System.currentTimeMillis();

      for (int i = 0; i < N; i++) {
        identifier.isOfThisType(originalValue);
      }

      long diff = System.currentTimeMillis() - startMillis;
      System.out.println(String.format("%s: %d operations took %d milliseconds (%f per op)",
          originalValue, N, diff, (double) diff / N));
      // Assert test always should finish in less than 10 seconds
      assertTrue(diff < 10000);
    }
  }
}
