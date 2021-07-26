/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.StringContains.containsString;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.apache.commons.lang3.StringUtils;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;
import com.ibm.whc.deid.models.Address;
import com.ibm.whc.deid.providers.identifiers.AddressIdentifier;
import com.ibm.whc.deid.providers.identifiers.Identifier;
import com.ibm.whc.deid.shared.pojo.config.masking.AddressMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.UnexpectedMaskingInputHandler;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderType;

public class AddressMaskingProviderTest extends TestLogSetUp implements MaskingProviderTest {

  /*
   * Tests the options and their boolean values (true and false). Note: Currently, the postalCode
   * mask = false and postal code nearest and nearestK options are not handled by the Address
   * provider.
   */

  protected static BasicMaskingProviderFactory maskingProviderFactory;

  @BeforeClass
  public static void setup() {
    maskingProviderFactory = new BasicMaskingProviderFactory();
  }

  protected BasicMaskingProviderFactory getMaskingProviderFactory() {
    return maskingProviderFactory;
  }

  @Test
  public void testMask() throws Exception {
    AddressIdentifier identifier = new AddressIdentifier();

    String validAddress = "200 E Main St, Phoenix AZ 85123, USA";
    Address originalAddress = identifier.parseAddress(validAddress);
    AddressMaskingProviderConfig maskingConfiguration = new AddressMaskingProviderConfig();

    AddressMaskingProvider addressMaskingProvider =
        (AddressMaskingProvider) getMaskingProviderFactory().getProviderFromType(
            MaskingProviderType.ADDRESS, null, maskingConfiguration, tenantId,
            localizationProperty);

    int randomizationOK = 0;

    for (int i = 0; i < 1000; i++) {
      String randomAddress = addressMaskingProvider.mask(validAddress);

      assertFalse(randomAddress.equals(validAddress));

      Address maskedAddress = identifier.parseAddress(randomAddress);

      if (maskedAddress == null) {
        System.out.println(randomAddress);
      }

      assertTrue(maskedAddress != null);

      if (!originalAddress.getNumber().equals(maskedAddress.getNumber())) {
        randomizationOK++;
      }
    }

    assertTrue(randomizationOK > 0);
  }

  @Test
  public void testMaskShortAddress() throws Exception {
    AddressMaskingProviderConfig maskingConfiguration = new AddressMaskingProviderConfig();
    AddressMaskingProvider addressMaskingProvider =
        (AddressMaskingProvider) getMaskingProviderFactory().getProviderFromType(
            MaskingProviderType.ADDRESS, null, maskingConfiguration, tenantId,
            localizationProperty);

    String validAddress = "200 E Main St";
    String randomAddress = addressMaskingProvider.mask(validAddress);
    assertFalse(randomAddress.equals(validAddress));
  }

  @Test
  public void testMaskPseudorandom() throws Exception {
    AddressMaskingProviderConfig maskingConfiguration = new AddressMaskingProviderConfig();
    maskingConfiguration.setMaskPseudorandom(true);
    AddressMaskingProvider addressMaskingProvider =
        (AddressMaskingProvider) getMaskingProviderFactory().getProviderFromType(
            MaskingProviderType.ADDRESS, null, maskingConfiguration, tenantId,
            localizationProperty);

    AddressIdentifier identifier = new AddressIdentifier();

    String[] validAddresses = {"200 E Main St, Phoenix AZ 85123, USA", "200 E Main St"};

    for (String validAddress : validAddresses) {
      String randomAddress = addressMaskingProvider.mask(validAddress);
      assertTrue(identifier.isOfThisType(randomAddress));
      assertFalse(randomAddress.equals(validAddress));

      for (int i = 0; i < 100; i++) {
        String rnd = addressMaskingProvider.mask(validAddress);
        assertEquals(randomAddress, rnd);
      }
    }
  }

  @Test
  public void testMaskPOBoxPseudorandom() throws Exception {
    AddressMaskingProviderConfig maskingConfiguration = new AddressMaskingProviderConfig();
    maskingConfiguration.setMaskPseudorandom(true);

    AddressMaskingProvider addressMaskingProvider =
        (AddressMaskingProvider) getMaskingProviderFactory().getProviderFromType(
            MaskingProviderType.ADDRESS, null, maskingConfiguration, tenantId,
            localizationProperty);
    AddressIdentifier identifier = new AddressIdentifier();

    String validAddress = "PO BOX 1234";
    String randomAddress = addressMaskingProvider.mask(validAddress);

    assertFalse(randomAddress.equals(validAddress));

    Address originalAddress = identifier.parseAddress(validAddress);
    Address maskedAddress = identifier.parseAddress(randomAddress);

    assertNotNull(maskedAddress);
    assertTrue(maskedAddress.isPOBox());
    assertFalse(maskedAddress.getPoBoxNumber().equals(originalAddress.getPoBoxNumber()));

    String firstRandom = randomAddress;

    for (int i = 0; i < 1000; i++) {
      String rnd = addressMaskingProvider.mask(validAddress);
      assertEquals(firstRandom, rnd);
    }
  }

  @Test
  public void testMaskPOBox() throws Exception {
    // In order to mitigate the rare chance of the PO Box number matching,
    // the logic is to count the number of times it matched and fail if it
    // matches more than once
    // If test case fails consistently with result matching more than once,
    // then the test tolerance has to be increased from >1 to something more
    // Chances Calculated to 2 failures for 10,000 runs
    AddressMaskingProviderConfig maskingConfiguration = new AddressMaskingProviderConfig();
    AddressMaskingProvider addressMaskingProvider =
        (AddressMaskingProvider) getMaskingProviderFactory().getProviderFromType(
            MaskingProviderType.ADDRESS, null, maskingConfiguration, tenantId,
            localizationProperty);
    AddressIdentifier identifier = new AddressIdentifier();

    String validAddress = "PO BOX 1234";
    String randomAddress = addressMaskingProvider.mask(validAddress);
    int numberMatched = 0;

    assertFalse(randomAddress.equals(validAddress));

    Address originalAddress = identifier.parseAddress(validAddress);
    Address maskedAddress = identifier.parseAddress(randomAddress);

    assertTrue(maskedAddress != null);
    assertTrue(maskedAddress.isPOBox());
    for (int i = 0; i < 100; i++) {
      if (maskedAddress.getPoBoxNumber().equals(originalAddress.getPoBoxNumber())) {
        numberMatched++;
      }
    }
    assertFalse(
        " PO Box number matched more than once. Number of times it matched: " + numberMatched,
        numberMatched > 2);
  }

  @Test
  public void testMaskNullAddressInputReturnNull() throws Exception {
    AddressMaskingProviderConfig maskingConfiguration = new AddressMaskingProviderConfig();

    AddressMaskingProvider addressMaskingProvider =
        (AddressMaskingProvider) getMaskingProviderFactory().getProviderFromType(
            MaskingProviderType.ADDRESS, null, maskingConfiguration, tenantId,
            localizationProperty);

    String invalidAddress = null;
    String randomAddress = addressMaskingProvider.mask(invalidAddress);

    assertEquals(null, randomAddress);

    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidAddressInputValidHandlingReturnNull() throws Exception {
    AddressMaskingProviderConfig maskingConfiguration = new AddressMaskingProviderConfig();
    maskingConfiguration.setUnspecifiedValueHandling(1);

    AddressMaskingProvider addressMaskingProvider =
        (AddressMaskingProvider) getMaskingProviderFactory().getProviderFromType(
            MaskingProviderType.ADDRESS, null, maskingConfiguration, tenantId,
            localizationProperty);

    String invalidAddress = "PA BOX 1234";
    String randomAddress = addressMaskingProvider.mask(invalidAddress);

    assertEquals(null, randomAddress);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidAddressInputValidHandlingReturnRandom() throws Exception {
    AddressMaskingProviderConfig maskingConfiguration = new AddressMaskingProviderConfig();
    maskingConfiguration.setUnspecifiedValueHandling(2);
    AddressMaskingProvider addressMaskingProvider =
        (AddressMaskingProvider) getMaskingProviderFactory().getProviderFromType(
            MaskingProviderType.ADDRESS, null, maskingConfiguration, tenantId,
            localizationProperty);
    Identifier identifier = new AddressIdentifier();

    String invalidAddress = "PA BOX 1234";
    String randomAddress = addressMaskingProvider.mask(invalidAddress);

    assertFalse(randomAddress.equals(invalidAddress));
    assertTrue(identifier.isOfThisType(randomAddress));
  }

  @Test
  public void testMaskInvalidAddressInputValidHandlingReturnRandomNew() throws Exception {
    AddressMaskingProviderConfig maskingConfiguration = new AddressMaskingProviderConfig();
    maskingConfiguration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.RANDOM);
    AddressMaskingProvider addressMaskingProvider =
        (AddressMaskingProvider) getMaskingProviderFactory().getProviderFromType(
            MaskingProviderType.ADDRESS, null, maskingConfiguration, tenantId,
            localizationProperty);
    Identifier identifier = new AddressIdentifier();

    String invalidAddress = "PA BOX 1234";
    String randomAddress = addressMaskingProvider.mask(invalidAddress);

    assertFalse(randomAddress.equals(invalidAddress));
    assertTrue(identifier.isOfThisType(randomAddress));
  }

  @Test
  public void testMaskInvalidAddressInputValidHandlingReturnDefaultCustomValue() throws Exception {
    AddressMaskingProviderConfig maskingConfiguration = new AddressMaskingProviderConfig();
    maskingConfiguration.setUnspecifiedValueHandling(3);
    AddressMaskingProvider addressMaskingProvider =
        (AddressMaskingProvider) getMaskingProviderFactory().getProviderFromType(
            MaskingProviderType.ADDRESS, null, maskingConfiguration, tenantId,
            localizationProperty);

    String invalidAddress = "PA BOX 1234";
    String randomAddress = addressMaskingProvider.mask(invalidAddress);

    assertEquals("OTHER", randomAddress);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidAddressInputValidHandlingReturnNonDefaultCustomValue()
      throws Exception {
    AddressMaskingProviderConfig maskingConfiguration = new AddressMaskingProviderConfig();
    maskingConfiguration.setUnspecifiedValueHandling(3);
    maskingConfiguration.setUnspecifiedValueReturnMessage("Test Address");
    AddressMaskingProvider addressMaskingProvider =
        (AddressMaskingProvider) getMaskingProviderFactory().getProviderFromType(
            MaskingProviderType.ADDRESS, null, maskingConfiguration, tenantId,
            localizationProperty);

    String invalidAddress = "PA BOX 1234";
    String randomAddress = addressMaskingProvider.mask(invalidAddress);

    assertEquals("Test Address", randomAddress);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidAddressInputInvalidHandlingReturnNull() throws Exception {
    AddressMaskingProviderConfig maskingConfiguration = new AddressMaskingProviderConfig();
    maskingConfiguration.setUnspecifiedValueHandling(4);
    AddressMaskingProvider addressMaskingProvider =
        (AddressMaskingProvider) getMaskingProviderFactory().getProviderFromType(
            MaskingProviderType.ADDRESS, null, maskingConfiguration, tenantId,
            localizationProperty);

    String invalidAddress = "PA BOX 1234";
    String randomAddress = addressMaskingProvider.mask(invalidAddress);

    assertEquals(null, randomAddress);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskNumberFalse() throws Exception {
    AddressMaskingProviderConfig maskingConfiguration = new AddressMaskingProviderConfig();
    maskingConfiguration.setNumberMask(false);

    AddressMaskingProvider addressMaskingProvider =
        (AddressMaskingProvider) getMaskingProviderFactory().getProviderFromType(
            MaskingProviderType.ADDRESS, null, maskingConfiguration, tenantId,
            localizationProperty);
    AddressIdentifier identifier = new AddressIdentifier();

    String validAddress = "200 E Main St, Phoenix AZ 85123, USA";
    String randomAddress = addressMaskingProvider.mask(validAddress);

    // System.out.println("=======> Mask Number False: origianl address [" +
    // validAddress + "], maskedAddress [" + randomAddress + "]" );
    assertFalse(randomAddress.equals(validAddress));

    Address originalAddress = identifier.parseAddress(validAddress);
    Address maskedAddress = identifier.parseAddress(randomAddress);

    if (originalAddress != null && maskedAddress != null
        && StringUtils.isNotBlank(originalAddress.getNumber())
        && StringUtils.isNotBlank(maskedAddress.getNumber())) {
      // System.out.println("=======> Mask Number False: origianl address
      // ["
      // + maskedAddress.getNumber() + "], maskedAddress [" +
      // originalAddress.getNumber() + "]" );
      assertTrue(maskedAddress.getNumber().equals(originalAddress.getNumber()));
    }
  }

  @Test
  public void testMaskStreetNameFalse() throws Exception {
    AddressMaskingProviderConfig maskingConfiguration = new AddressMaskingProviderConfig();
    maskingConfiguration.setStreetNameMask(false);

    AddressMaskingProvider addressMaskingProvider =
        (AddressMaskingProvider) getMaskingProviderFactory().getProviderFromType(
            MaskingProviderType.ADDRESS, null, maskingConfiguration, tenantId,
            localizationProperty);
    AddressIdentifier identifier = new AddressIdentifier();

    String validAddress = "200 E Main St, Phoenix AZ 85123, USA";
    String randomAddress = addressMaskingProvider.mask(validAddress);

    assertFalse(randomAddress.equals(validAddress));

    Address originalAddress = identifier.parseAddress(validAddress);
    Address maskedAddress = identifier.parseAddress(randomAddress);
    assertNotNull(originalAddress);
    assertNotNull(maskedAddress);
    assertEquals(originalAddress.getName(), maskedAddress.getName());
  }

  @Test
  public void testMaskRoadTypeFalse() throws Exception {
    AddressMaskingProviderConfig maskingConfiguration = new AddressMaskingProviderConfig();
    maskingConfiguration.setRoadTypeMask(false);
    maskingConfiguration.setPostalCodeNearest(false);
    maskingConfiguration.setMaskPseudorandom(false);

    AddressMaskingProvider addressMaskingProvider =
        (AddressMaskingProvider) getMaskingProviderFactory().getProviderFromType(
            MaskingProviderType.ADDRESS, null, maskingConfiguration, tenantId,
            localizationProperty);
    AddressIdentifier identifier = new AddressIdentifier();

    String validAddress = "200 E Main St, Phoenix AZ 85123, USA";
    String randomAddress = addressMaskingProvider.mask(validAddress);

    assertNotEquals(validAddress, randomAddress);

    Address originalAddress = identifier.parseAddress(validAddress);
    Address maskedAddress = identifier.parseAddress(randomAddress);
    assertNotNull(originalAddress);
    assertNotNull(maskedAddress);
    assertEquals(originalAddress.getRoadType(), maskedAddress.getRoadType());
  }

  @Test
  public void testMaskCityFalse() throws Exception {
    AddressMaskingProviderConfig maskingConfiguration = new AddressMaskingProviderConfig();
    maskingConfiguration.setCityMask(false);

    AddressMaskingProvider addressMaskingProvider =
        (AddressMaskingProvider) getMaskingProviderFactory().getProviderFromType(
            MaskingProviderType.ADDRESS, null, maskingConfiguration, tenantId,
            localizationProperty);
    AddressIdentifier identifier = new AddressIdentifier();

    String validAddress = "200 E Main St, Phoenix AZ 85123, USA";
    String randomAddress = addressMaskingProvider.mask(validAddress);

    // System.out.println("=======> Mask City False: origianl address [" +
    // validAddress + "], maskedAddress [" + randomAddress + "]" );
    assertFalse(randomAddress.equals(validAddress));

    Address originalAddress = identifier.parseAddress(validAddress);
    Address maskedAddress = identifier.parseAddress(randomAddress);
    if (originalAddress != null && maskedAddress != null
        && StringUtils.isNotBlank(originalAddress.getCityOrState())
        && StringUtils.isNotBlank(maskedAddress.getCityOrState())) {
      // System.out.println("=======> Mask City False: origianl address ["
      // + maskedAddress.getCityOrState() + "], maskedAddress [" +
      // originalAddress.getCityOrState() + "]" );
      assertTrue(maskedAddress.getCityOrState().equals(originalAddress.getCityOrState()));
    }
  }

  @Test
  public void testMaskCountryFalse() throws Exception {
    AddressMaskingProviderConfig maskingConfiguration = new AddressMaskingProviderConfig();
    maskingConfiguration.setCountryMask(false);

    AddressMaskingProvider addressMaskingProvider =
        (AddressMaskingProvider) getMaskingProviderFactory().getProviderFromType(
            MaskingProviderType.ADDRESS, null, maskingConfiguration, tenantId,
            localizationProperty);
    AddressIdentifier identifier = new AddressIdentifier();

    String validAddress = "200 E Main St, Phoenix AZ 85123, USA";
    String randomAddress = addressMaskingProvider.mask(validAddress);

    // System.out.println("=======> Mask Country False: origianl address ["
    // + validAddress + "], maskedAddress [" + randomAddress + "]" );
    assertFalse(randomAddress.equals(validAddress));

    Address originalAddress = identifier.parseAddress(validAddress);
    Address maskedAddress = identifier.parseAddress(randomAddress);
    if (originalAddress != null && maskedAddress != null
        && StringUtils.isNotBlank(originalAddress.getCountry())
        && StringUtils.isNotBlank(maskedAddress.getCountry())) {
      // System.out.println("=======> Mask Country False: origianl address
      // ["
      // + maskedAddress.getCountry() + "], maskedAddress [" +
      // originalAddress.getCountry() + "]" );
      assertTrue(maskedAddress.getCountry().equals(originalAddress.getCountry()));
    }
  }

  @Test
  @Ignore
  public void testMaskPostalCodeFalse() throws Exception {
    // address.postalCode.mask is not currently handled by the provider
    AddressMaskingProviderConfig maskingConfiguration = new AddressMaskingProviderConfig();
    maskingConfiguration.setPostalCodeMask(false);

    AddressMaskingProvider addressMaskingProvider =
        (AddressMaskingProvider) getMaskingProviderFactory().getProviderFromType(
            MaskingProviderType.ADDRESS, null, maskingConfiguration, tenantId,
            localizationProperty);
    AddressIdentifier identifier = new AddressIdentifier();

    String validAddress = "PO BOX 1234";
    String randomAddress = addressMaskingProvider.mask(validAddress);

    // System.out.println("=======> Mask Postal Code False: origianl address
    // ["
    // + validAddress + "], maskedAddress [" + randomAddress + "]" );
    assertFalse(randomAddress.equals(validAddress));

    Address originalAddress = identifier.parseAddress(validAddress);
    Address maskedAddress = identifier.parseAddress(randomAddress);
    if (originalAddress != null && maskedAddress != null
        && StringUtils.isNotBlank(originalAddress.getPostalCode())
        && StringUtils.isNotBlank(maskedAddress.getPostalCode())) {
      assertTrue(maskedAddress.getPostalCode().equals(originalAddress.getPostalCode()));
    }
  }

  @Test
  public void testMaskPostalCodeNearest() throws Exception {
    // address.postalCode.nearest, nearestK are not currently handled
    AddressMaskingProviderConfig maskingConfiguration = new AddressMaskingProviderConfig();
    maskingConfiguration.setPostalCodeNearest(true);
    String originalValue = "200 E Main St, Phoenix AZ 85123, USA";
    AddressMaskingProvider addressMaskingProvider =
        (AddressMaskingProvider) getMaskingProviderFactory().getProviderFromType(
            MaskingProviderType.ADDRESS, null, maskingConfiguration, tenantId,
            localizationProperty);
    AddressIdentifier identifier = new AddressIdentifier();

    // postal codes near 85123
    String[] nearestPostalCodes =
        {"85130", "85128", "85194", "85145", "85123", "85131", "85122", "85141", "85193"};
    List<String> nearestPostalCodeList = new ArrayList<>(Arrays.asList(nearestPostalCodes));
    int maskOK = 0;

    for (int i = 0; i < 100; i++) {
      String maskedValue = addressMaskingProvider.mask(originalValue);
      // System.out.println("=======> PostalCodeNearest: originalValue ["
      // + originalValue + "], maskedValue [" + maskedValue + "]");
      assertFalse(originalValue.equals(maskedValue));
      Address maskedAddress = identifier.parseAddress(maskedValue);

      if (maskedAddress != null && StringUtils.isNotBlank(maskedAddress.getPostalCode())) {
        if (nearestPostalCodeList.contains(maskedAddress.getPostalCode())) {
          // System.out.println("=======> PostalCodeNearest: masked
          // postal code ["
          // + maskedAddress.getPostalCode() + "]");
          maskOK++;
        }
      } else {
        for (String nearestPostalCode : nearestPostalCodeList) {
          if (maskedValue.contains(nearestPostalCode)) {
            // System.out.println("=======> PostalCodeNearest:
            // postal code contains ["
            // + nearestPostalCode + "]");
            maskOK++;
          }
        }
      }
    }
    assertTrue(maskOK > 0);
  }

  @Test
  @Ignore
  public void testPerformance() {
    int N = 1000000;

    AddressMaskingProviderConfig defaultConfiguration = new AddressMaskingProviderConfig();
    defaultConfiguration.setPostalCodeNearest(true);

    AddressMaskingProviderConfig nearestConfiguration = new AddressMaskingProviderConfig();
    nearestConfiguration.setPostalCodeNearest(true);

    AddressMaskingProviderConfig[] configurations =
        new AddressMaskingProviderConfig[] {defaultConfiguration, nearestConfiguration};

    String[] originalValues = new String[] {"200 E Main St, Phoenix AZ 85123, USA", "PO BOX 1234"};

    for (AddressMaskingProviderConfig maskingConfiguration : configurations) {
      AddressMaskingProvider addressMaskingProvider =
          (AddressMaskingProvider) getMaskingProviderFactory().getProviderFromType(
              MaskingProviderType.ADDRESS, null, maskingConfiguration, tenantId,
              localizationProperty);

      for (String originalValue : originalValues) {
        long startMillis = System.currentTimeMillis();

        for (int i = 0; i < N; i++) {
          addressMaskingProvider.mask(originalValue);
        }

        long diff = System.currentTimeMillis() - startMillis;
        System.out.println(String.format(" %s: %d operations took %d milliseconds (%f per op)",
            originalValue, N, diff, (double) diff / N));
        // Assert test always should finish in less than 3 minutes
        assertTrue(diff < 180000);
      }
    }
  }

}
