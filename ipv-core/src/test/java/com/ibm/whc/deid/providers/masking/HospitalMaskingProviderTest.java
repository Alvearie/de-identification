/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import java.util.Arrays;
import java.util.HashSet;
import org.junit.Test;
import com.ibm.whc.deid.shared.pojo.config.masking.HospitalMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.UnexpectedMaskingInputHandler;

public class HospitalMaskingProviderTest implements MaskingProviderTest {

  /*
   * Tests for preserveCountry option and its boolean values (true and false). It also tests for the
   * localization of the hospital.
   */

  public static final HashSet<String> TEST_HOSPITALS_US =
      new HashSet<>(Arrays.asList("Jarrett Hospital", "Susan Clinic", "Olmsted Clinic"));
  public static final HashSet<String> TEST_HOSPITALS_CA = new HashSet<>(
      Arrays.asList("Dumas Urgent Care", "Denis Medical Group", "Chris Community Hospital"));

  @Test
  public void testMask() {
    HospitalMaskingProviderConfig configuration = new HospitalMaskingProviderConfig();
    configuration.setMaskPreserveCountry(false);
    HospitalMaskingProvider maskingProvider =
        new HospitalMaskingProvider(configuration, tenantId, localizationProperty);

    boolean changed = false;
    String hospitalName = "Hospital San Francisco";
    for (int i = 0; i < 20; i++) {
      String maskedValue = maskingProvider.mask(hospitalName);
      assertNotNull(maskedValue);
      assertNotEquals("OTHER", maskedValue);
      maskingProvider.getHospitalManager().isValidKey(maskedValue);
      if (!hospitalName.equalsIgnoreCase(maskedValue)) {
        changed = true;
      }
    }
    assertTrue(changed);

    hospitalName = "XXXXX";
    for (int i = 0; i < 20; i++) {
      String maskedValue = maskingProvider.mask(hospitalName);
      assertNotNull(maskedValue);
      assertNotEquals("OTHER", maskedValue);
      maskingProvider.getHospitalManager().isValidKey(maskedValue);
    }
  }
  
  @Test
  public void testMask_multiCountry() {
    HospitalMaskingProviderConfig configuration = new HospitalMaskingProviderConfig();
    configuration.setMaskPreserveCountry(false);
    HospitalMaskingProvider maskingProvider =
        new HospitalMaskingProvider(configuration, tenantId, TEST_LOCALIZATION_PROPERTIES);

    boolean changed = false;
    String hospitalName = TEST_HOSPITALS_US.iterator().next();
    for (int i = 0; i < 100; i++) {
      String maskedValue = maskingProvider.mask(hospitalName);
      assertTrue(maskedValue,
          TEST_HOSPITALS_US.contains(maskedValue) || TEST_HOSPITALS_CA.contains(maskedValue));
      if (!hospitalName.equals(maskedValue)) {
        changed = true;
      }
    }
    assertTrue(changed);

    hospitalName = "unknown";
    for (int i = 0; i < 100; i++) {
      String maskedValue = maskingProvider.mask(hospitalName);
      assertTrue(maskedValue,
          TEST_HOSPITALS_US.contains(maskedValue) || TEST_HOSPITALS_CA.contains(maskedValue));
    }
  }

  @Test
  public void testMaskPreserveCountry() {
    HospitalMaskingProviderConfig configuration = new HospitalMaskingProviderConfig();
    // the preserve country option by default is true.
    HospitalMaskingProvider maskingProvider =
        new HospitalMaskingProvider(configuration, tenantId, TEST_LOCALIZATION_PROPERTIES);

    boolean changed = false;
    String hospitalName = TEST_HOSPITALS_US.iterator().next();
    for (int i = 0; i < 100; i++) {
      String maskedValue = maskingProvider.mask(hospitalName);
      assertTrue(maskedValue, TEST_HOSPITALS_US.contains(maskedValue));
      if (!hospitalName.equals(maskedValue)) {
        changed = true;
      }
    }
    assertTrue(changed);

    changed = false;
    hospitalName = TEST_HOSPITALS_CA.iterator().next();
    for (int i = 0; i < 100; i++) {
      String maskedValue = maskingProvider.mask(hospitalName);
      assertTrue(maskedValue, TEST_HOSPITALS_CA.contains(maskedValue));
      if (!hospitalName.equals(maskedValue)) {
        changed = true;
      }
    }
    assertTrue(changed);
  }

  @Test
  public void testMaskInvalidHospitalInputValidHandlingReturnNull() throws Exception {
    HospitalMaskingProviderConfig configuration = new HospitalMaskingProviderConfig();
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.NULL);
    MaskingProvider maskingProvider =
        new HospitalMaskingProvider(configuration, tenantId, localizationProperty);

    String invalidHospital = "Invalid Hospital";
    String maskedHospital = maskingProvider.mask(invalidHospital);

    assertEquals(null, maskedHospital);
  }

  @Test
  public void testMaskInvalidHospitalInputValidHandlingReturnRandom() throws Exception {
    HospitalMaskingProviderConfig configuration = new HospitalMaskingProviderConfig();
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.RANDOM);
    MaskingProvider maskingProvider =
        new HospitalMaskingProvider(configuration, tenantId, TEST_LOCALIZATION_PROPERTIES);

    String invalidHospital = "Invalid Hospital";
    String maskedHospital = maskingProvider.mask(invalidHospital);
    assertTrue(maskedHospital,
        TEST_HOSPITALS_US.contains(maskedHospital) || TEST_HOSPITALS_CA.contains(maskedHospital));
  }

  @Test
  public void testMaskInvalidHospitalInputValidHandlingReturnCustomValue() throws Exception {
    HospitalMaskingProviderConfig configuration = new HospitalMaskingProviderConfig();
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.MESSAGE);
    configuration.setUnexpectedInputReturnMessage("New Hospital");
    MaskingProvider maskingProvider =
        new HospitalMaskingProvider(configuration, tenantId, localizationProperty);

    String invalidHospital = "Invalid Hospital";
    String maskedHospital = maskingProvider.mask(invalidHospital);
    assertEquals("New Hospital", maskedHospital);
  }

  @Test
  public void testMaskInvalidHospitalInputValidHandlingReturnDefaultCustomValue()
      throws Exception {
    HospitalMaskingProviderConfig configuration = new HospitalMaskingProviderConfig();
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.MESSAGE);
    configuration.setUnspecifiedValueReturnMessage("Test Hospital");
    MaskingProvider maskingProvider =
        new HospitalMaskingProvider(configuration, tenantId, localizationProperty);

    String invalidHospital = "Invalid Hospital";
    String maskedHospital = maskingProvider.mask(invalidHospital);
    assertEquals("OTHER", maskedHospital);
  }

  @Test
  public void testMaskInvalidHospitalInputValidHandlingExit() throws Exception {
    HospitalMaskingProviderConfig configuration = new HospitalMaskingProviderConfig();
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.ERROR_EXIT);
    configuration.setUnspecifiedValueReturnMessage("Test Hospital");
    MaskingProvider maskingProvider =
        new HospitalMaskingProvider(configuration, tenantId, localizationProperty);

    String invalidHospital = "Invalid Hospital";
    try {
      maskingProvider.mask(invalidHospital);
    } catch (PrivacyProviderInvalidInputException e) {
      assertFalse(e.getMessage().contains(invalidHospital));
    }
  }
}
