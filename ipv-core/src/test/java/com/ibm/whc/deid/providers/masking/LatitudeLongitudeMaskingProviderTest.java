/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.StringContains.containsString;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertTrue;
import org.junit.Ignore;
import org.junit.Test;
import com.ibm.whc.deid.providers.identifiers.Identifier;
import com.ibm.whc.deid.providers.identifiers.LatitudeLongitudeIdentifier;
import com.ibm.whc.deid.shared.pojo.config.masking.LatitudeLongitudeMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.UnexpectedMaskingInputHandler;

public class LatitudeLongitudeMaskingProviderTest extends TestLogSetUp {
  /*
   * Test for all boolean options and their true and false values and with various minimum and
   * maximum offset radius values. Also, tests for invalid value.
   */

  @Test
  public void testMaskWithinCircle() throws Exception {
    // By default randomWithinCircle option is true and others are false
    MaskingProvider latlonMaskingProvider = new LatitudeLongitudeMaskingProvider();
    LatitudeLongitudeIdentifier latitudeLongitudeIdentifier = new LatitudeLongitudeIdentifier();

    String gpsCoords = "10.0000,12.0000";
    String maskedResult = latlonMaskingProvider.mask(gpsCoords);
    // System.out.println("=======> randomWithinCircle = true, gpsCoords ["
    // + gpsCoords + ", maskedResult [" + maskedResult + "}" );
    assertFalse(maskedResult.equals(gpsCoords));
    assertTrue(latitudeLongitudeIdentifier.isOfThisType(maskedResult));
    assertTrue(latitudeLongitudeIdentifier.isGPSFormat(maskedResult));

    String compassCoords = "N90.00.00 E180.00.00";
    maskedResult = latlonMaskingProvider.mask(compassCoords);
    // System.out.println("=======> randomWithinCircle = true, compassCoords
    // ["
    // + compassCoords + ", maskedResult [" + maskedResult + "}" );
    assertFalse(maskedResult.equals(compassCoords));
    assertTrue(latitudeLongitudeIdentifier.isOfThisType(maskedResult));
    assertTrue(latitudeLongitudeIdentifier.isCompassFormat(maskedResult));

    String dmsCoords = "12:30'23.256547S 12:30'23.256547E";
    // System.out.println("=======> randomWithinCircle = true, dmsCoords ["
    // + dmsCoords + ", maskedResult [" + maskedResult + "}" );
    assertTrue(latitudeLongitudeIdentifier.isOfThisType(dmsCoords));
    maskedResult = latlonMaskingProvider.mask(dmsCoords);
    assertFalse(maskedResult.equals(dmsCoords));
    assertTrue(latitudeLongitudeIdentifier.isOfThisType(maskedResult));
    assertTrue(latitudeLongitudeIdentifier.isDMSFormat(maskedResult));
  }

  @Test
  public void testMaskFixedRadiusRandomDirection() throws Exception {
    LatitudeLongitudeMaskingProviderConfig configuration =
        new LatitudeLongitudeMaskingProviderConfig();
    configuration.setMaskFixedRadiusRandomDirection(true);
    configuration.setMaskRandomWithinCircle(false);
    MaskingProvider latlonMaskingProvider = new LatitudeLongitudeMaskingProvider(configuration);
    LatitudeLongitudeIdentifier latitudeLongitudeIdentifier = new LatitudeLongitudeIdentifier();

    String gpsCoords = "10.0000,12.0000";
    String maskedResult = latlonMaskingProvider.mask(gpsCoords);
    // System.out.println("=======> fixedRadiusRandomDirection = true,
    // gpsCoords ["
    // + gpsCoords + ", maskedResult [" + maskedResult + "}" );

    assertFalse(maskedResult.equals(gpsCoords));
    assertTrue(latitudeLongitudeIdentifier.isOfThisType(maskedResult));
    assertTrue(latitudeLongitudeIdentifier.isGPSFormat(maskedResult));

    String compassCoords = "N90.00.00 E180.00.00";
    maskedResult = latlonMaskingProvider.mask(compassCoords);
    // System.out.println("=======> fixedRadiusRandomDirection = true,
    // compassCoords ["
    // + compassCoords + ", maskedResult [" + maskedResult + "}" );

    assertFalse(maskedResult.equals(compassCoords));
    assertTrue(maskedResult, latitudeLongitudeIdentifier.isOfThisType(maskedResult));
    assertTrue(latitudeLongitudeIdentifier.isCompassFormat(maskedResult));

    String dmsCoords = "12:30'23.256547S 12:30'23.256547E";
    assertTrue(latitudeLongitudeIdentifier.isOfThisType(dmsCoords));
    maskedResult = latlonMaskingProvider.mask(dmsCoords);
    // System.out.println("=======> fixedRadiusRandomDirection = true,
    // dmsCoords ["
    // + dmsCoords + ", maskedResult [" + maskedResult + "}" );

    assertFalse(maskedResult.equals(dmsCoords));
    assertTrue(latitudeLongitudeIdentifier.isOfThisType(maskedResult));
    assertTrue(latitudeLongitudeIdentifier.isDMSFormat(maskedResult));
  }

  @Test
  public void testMaskDonut() throws Exception {
    LatitudeLongitudeMaskingProviderConfig configuration =
        new LatitudeLongitudeMaskingProviderConfig();
    configuration.setMaskRandomWithinCircle(false);
    configuration.setMaskDonutMasking(true);
    configuration.setOffsetMaximumRadius(500);
    configuration.setOffsetMinimumRadius(100);

    MaskingProvider latlonMaskingProvider = new LatitudeLongitudeMaskingProvider(configuration);
    LatitudeLongitudeIdentifier latitudeLongitudeIdentifier = new LatitudeLongitudeIdentifier();

    String gpsCoords = "10.0000,12.0000";
    String maskedResult = latlonMaskingProvider.mask(gpsCoords);
    // System.out.println("=======> donutMasking = true, gpsCoords [" +
    // gpsCoords + ", maskedResult [" + maskedResult + "}" );

    assertFalse(maskedResult.equals(gpsCoords));
    assertTrue(latitudeLongitudeIdentifier.isOfThisType(maskedResult));
    assertTrue(latitudeLongitudeIdentifier.isGPSFormat(maskedResult));

    String compassCoords = "N90.00.00 E180.00.00";
    maskedResult = latlonMaskingProvider.mask(compassCoords);
    // System.out.println("=======> donutMasking = true, compassCoords [" +
    // compassCoords + ", maskedResult [" + maskedResult + "}" );

    assertFalse(maskedResult.equals(compassCoords));
    assertTrue(latitudeLongitudeIdentifier.isOfThisType(maskedResult));
    assertTrue(latitudeLongitudeIdentifier.isCompassFormat(maskedResult));

    String dmsCoords = "12:30'23.256547S 12:30'23.256547E";
    assertTrue(latitudeLongitudeIdentifier.isOfThisType(dmsCoords));
    maskedResult = latlonMaskingProvider.mask(dmsCoords);
    // System.out.println("=======> donutMasking = true, dmsCoords [" +
    // dmsCoords + ", maskedResult [" + maskedResult + "}" );

    assertFalse(maskedResult.equals(dmsCoords));
    assertTrue(latitudeLongitudeIdentifier.isOfThisType(maskedResult));
    assertTrue(latitudeLongitudeIdentifier.isDMSFormat(maskedResult));
  }

  @Test
  public void testMask() throws Exception {
    // By default randomWithinCircle option is true and others are false
    LatitudeLongitudeMaskingProviderConfig configuration =
        new LatitudeLongitudeMaskingProviderConfig();
    configuration.setMaskRandomWithinCircle(false);
    MaskingProvider latlonMaskingProvider = new LatitudeLongitudeMaskingProvider(configuration);
    LatitudeLongitudeIdentifier latitudeLongitudeIdentifier = new LatitudeLongitudeIdentifier();

    String gpsCoords = "10.0000,12.0000";
    for (int i = 0; i < 20; i++) {
      String maskedResult = latlonMaskingProvider.mask(gpsCoords);
      // System.out.println("=======> randomWithinCircle = true, gpsCoords ["
      // + gpsCoords + ", maskedResult [" + maskedResult + "}" );
      assertNotEquals(gpsCoords, maskedResult);
      assertTrue(latitudeLongitudeIdentifier.isOfThisType(maskedResult));
      assertTrue(latitudeLongitudeIdentifier.isGPSFormat(maskedResult));
    }

    for (int i = 0; i < 20; i++) {
      String compassCoords = "N90.00.00 E180.00.00";
      String maskedResult = latlonMaskingProvider.mask(compassCoords);
      // System.out.println("=======> randomWithinCircle = true, compassCoords
      // ["
      // + compassCoords + ", maskedResult [" + maskedResult + "}" );
      assertNotEquals(compassCoords, maskedResult);
      assertTrue(latitudeLongitudeIdentifier.isOfThisType(maskedResult));
      assertTrue(latitudeLongitudeIdentifier.isCompassFormat(maskedResult));
    }

    for (int i = 0; i < 20; i++) {
      String dmsCoords = "12:30'23.256547S 12:30'23.256547E";
      // System.out.println("=======> randomWithinCircle = true, dmsCoords ["
      // + dmsCoords + ", maskedResult [" + maskedResult + "}" );
      String maskedResult = latlonMaskingProvider.mask(dmsCoords);
      assertNotEquals(dmsCoords, maskedResult);
      assertTrue(maskedResult, latitudeLongitudeIdentifier.isOfThisType(maskedResult));
      assertTrue(maskedResult, latitudeLongitudeIdentifier.isDMSFormat(maskedResult));
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void testAssertThatInvalidArgumentIsCaught() throws Exception {
    LatitudeLongitudeMaskingProviderConfig configuration =
        new LatitudeLongitudeMaskingProviderConfig();
    configuration.setOffsetMinimumRadius(5);
    new LatitudeLongitudeMaskingProvider(configuration);
  }

  @Test
  public void testMaskNullLatitudeLongitudeInputReturnNull() throws Exception {
    LatitudeLongitudeMaskingProviderConfig configuration =
        new LatitudeLongitudeMaskingProviderConfig();
    MaskingProvider maskingProvider = new LatitudeLongitudeMaskingProvider(configuration);

    String invalidLatitudeLongitude = null;
    String maskedLatitudeLongitude = maskingProvider.mask(invalidLatitudeLongitude);

    assertEquals(null, maskedLatitudeLongitude);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidLatitudeLongitudeInputValidHandlingReturnNull() throws Exception {
    LatitudeLongitudeMaskingProviderConfig configuration =
        new LatitudeLongitudeMaskingProviderConfig();
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.NULL);
    MaskingProvider maskingProvider = new LatitudeLongitudeMaskingProvider(configuration);

    String invalidLatitudeLongitude = "Invalid Latitude Longitude";
    String maskedLatitudeLongitude = maskingProvider.mask(invalidLatitudeLongitude);

    assertEquals(null, maskedLatitudeLongitude);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidLatitudeLongitudeInputValidHandlingReturnRandom() throws Exception {
    LatitudeLongitudeMaskingProviderConfig configuration =
        new LatitudeLongitudeMaskingProviderConfig();
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.RANDOM);
    MaskingProvider maskingProvider = new LatitudeLongitudeMaskingProvider(configuration);
    Identifier identifier = new LatitudeLongitudeIdentifier();

    String invalidLatitudeLongitude = "Invalid Latitude Longitude";
    String maskedLatitudeLongitude = maskingProvider.mask(invalidLatitudeLongitude);

    assertFalse(maskedLatitudeLongitude.equals(invalidLatitudeLongitude));
    assertTrue(identifier.isOfThisType(maskedLatitudeLongitude));
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidLatitudeLongitudeInputValidHandlingReturnDefaultCustomValue()
      throws Exception {
    LatitudeLongitudeMaskingProviderConfig configuration =
        new LatitudeLongitudeMaskingProviderConfig();
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.MESSAGE);
    MaskingProvider maskingProvider = new LatitudeLongitudeMaskingProvider(configuration);

    String invalidLatitudeLongitude = "Invalid Latitude Longitude";
    String maskedLatitudeLongitude = maskingProvider.mask(invalidLatitudeLongitude);

    assertEquals("OTHER", maskedLatitudeLongitude);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidLatitudeLongitudeInputValidHandlingReturnNonDefaultCustomValue()
      throws Exception {
    LatitudeLongitudeMaskingProviderConfig configuration =
        new LatitudeLongitudeMaskingProviderConfig();
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.MESSAGE);
    configuration.setUnexpectedInputReturnMessage("Test Latitude Longitude");
    MaskingProvider maskingProvider = new LatitudeLongitudeMaskingProvider(configuration);

    String invalidLatitudeLongitude = "Invalid Latitude Longitude";
    String maskedLatitudeLongitude = maskingProvider.mask(invalidLatitudeLongitude);

    assertEquals("Test Latitude Longitude", maskedLatitudeLongitude);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  @Ignore
  public void testPerformance() {
    int N = 1000000;
    LatitudeLongitudeMaskingProviderConfig defaultConfiguration =
        new LatitudeLongitudeMaskingProviderConfig();

    LatitudeLongitudeMaskingProviderConfig[] configurations =
        new LatitudeLongitudeMaskingProviderConfig[] {defaultConfiguration};

    String[] originalValues = new String[] {"10.0000,12.0000"};

    for (LatitudeLongitudeMaskingProviderConfig maskingConfiguration : configurations) {
      LatitudeLongitudeMaskingProvider maskingProvider =
          new LatitudeLongitudeMaskingProvider(maskingConfiguration);

      for (String originalValue : originalValues) {
        long startMillis = System.currentTimeMillis();

        for (int i = 0; i < N; i++) {
          maskingProvider.mask(originalValue);
        }

        long diff = System.currentTimeMillis() - startMillis;
        System.out.println(String.format("%s: %d operations took %d milliseconds (%f per op)",
            originalValue, N, diff, (double) diff / N));
        // Assert test always should finish in less than 10 seconds
        assertTrue(diff < 10000);
      }
    }
  }
}
