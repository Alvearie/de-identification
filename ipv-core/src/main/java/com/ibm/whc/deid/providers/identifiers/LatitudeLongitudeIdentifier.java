/*
 * © Merative US L.P. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import com.ibm.whc.deid.models.LatitudeLongitude;
import com.ibm.whc.deid.models.LatitudeLongitudeFormat;
import com.ibm.whc.deid.models.ValueClass;
import com.ibm.whc.deid.providers.ProviderType;
import com.ibm.whc.deid.util.GeoUtils;

/**
 * The type Latitude longitude identifier.
 *
 */
public class LatitudeLongitudeIdentifier extends AbstractRegexBasedIdentifier {

  private static final long serialVersionUID = 9183031051457340625L;

  private static final String[] appropriateNames = {"Latitude", "Longitude", "LatitudeLongitude"};
  private static final Pattern latlonPattern = Pattern.compile(
      "^-?([1-8]?[0-9]\\.{1}\\d{1,8}|90\\.{1}0{1,6}),\\s*-?((([1]?[0-7][0-9]|[1-9]?[0-9])\\.{1}\\d{1,8}$)|[1]?[1-8][0]\\.{1}0{1,6})$");
  private static final Pattern compassPattern = Pattern.compile(
      "^(?<ns>[NS])(((?<nsDegrees>[0-8][0-9])\\.(?<nsMinutes>[0-5]\\d)\\.(?<nsSeconds>[0-5]\\d))|((?<nsDegrees2>90)(?<nsMinutes2>\\.00)(?<nsSeconds2>\\.00)))[ |,](?<ew>[EW])(((?<ewDegrees>(0\\d\\d|1[0-7]\\d))\\.(?<ewMinutes>[0-5]\\d)\\.(?<ewSeconds>[0-5]\\d))|((?<ewDegrees2>180)\\.(?<ewMinutes2>00)\\.(?<ewSeconds2>00)))$");
  private static final Pattern dmsCoordinatePattern = Pattern.compile(
      "(?<nsDegrees>[0-9]{1,2})[:|°](?<nsMinutes>[0-9]{1,2})[:|'](?<nsSeconds>(?:\\b[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+\\b))\"?(?<ns>[N|S])[ |,](?<ewDegrees>(1[0-7]\\d|0?[0-9]{1,2}))[:|°](?<ewMinutes>[0-9]{1,2})[:|'](?<ewSeconds>(?:\\b[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+\\b))\"?(?<ew>[E|W])",
      Pattern.UNICODE_CASE);

  private Collection<Pattern> coordinatePatterns =
      new ArrayList<>(Arrays.asList(latlonPattern, compassPattern, dmsCoordinatePattern));

  @Override
  public ProviderType getType() {
    return ProviderType.LATITUDE_LONGITUDE;
  }

  @Override
  public String getDescription() {
    return "Latitude/longitude identification. Supports GPS and DMS coordinates formats";
  }

  @Override
  public ValueClass getValueClass() {
    return ValueClass.LOCATION;
  }

  /**
   * Gets pattern from format.
   *
   * @param format the format
   * @return the pattern from format
   */
  public Pattern getPatternFromFormat(LatitudeLongitudeFormat format) {
    if (format == LatitudeLongitudeFormat.COMPASS) {
      return compassPattern;
    } else if (format == LatitudeLongitudeFormat.DECIMAL) {
      return latlonPattern;
    } else if (format == LatitudeLongitudeFormat.DMS) {
      return dmsCoordinatePattern;
    }
    return null;
  }

  /**
   * Is dms format boolean.
   *
   * @param identifier the identifier
   * @return the boolean
   */
  public boolean isDMSFormat(String identifier) {
    return dmsCoordinatePattern.matcher(identifier).matches();
  }

  /**
   * Is compass format boolean.
   *
   * @param identifier the identifier
   * @return the boolean
   */
  public boolean isCompassFormat(String identifier) {
    return compassPattern.matcher(identifier).matches();
  }

  /**
   * Is gps format boolean.
   *
   * @param identifier the identifier
   * @return the boolean
   */
  public boolean isGPSFormat(String identifier) {
    return latlonPattern.matcher(identifier).matches();
  }

  /**
   * Parse coordinate latitude longitude.
   *
   * @param identifier the identifier
   * @return the latitude longitude
   */
  public LatitudeLongitude parseCoordinate(String identifier) {
    if (isGPSFormat(identifier)) {
      return parseGPSFormat(identifier);
    } else if (isCompassFormat(identifier) || isDMSFormat(identifier)) {
      return parseCompassFormat(identifier);
    }
    return null;
  }

  /**
   * Parse gps format latitude longitude.
   *
   * @param identifier the identifier
   * @return the latitude longitude
   */
  public static LatitudeLongitude parseGPSFormat(String identifier) {
    // GPS format is "x,y"
    String[] parts = identifier.split(",");
    double latitude = Double.parseDouble(parts[0]);
    double longitude = Double.parseDouble(parts[1]);
    return new LatitudeLongitude(latitude, longitude, LatitudeLongitudeFormat.DECIMAL);
  }

  /**
   * Parse compass format latitude longitude.
   *
   * @param identifier the identifier
   * @return the latitude longitude
   */
  public LatitudeLongitude parseCompassFormat(String identifier) {
    double latitude;
    double longitude;
    double nsDegrees, ewDegrees;
    double nsMinutes, ewMinutes;
    double nsSeconds, ewSeconds;
    String ns = null;
    String ew = null;
    LatitudeLongitudeFormat format = LatitudeLongitudeFormat.COMPASS;

    Matcher m1 = getPatternFromFormat(LatitudeLongitudeFormat.DMS).matcher(identifier);
    if (m1.matches()) {
      nsDegrees = Double.parseDouble(m1.group("nsDegrees"));
      nsMinutes = Double.parseDouble(m1.group("nsMinutes"));
      nsSeconds = Double.parseDouble(m1.group("nsSeconds"));

      ewDegrees = Double.parseDouble(m1.group("ewDegrees"));
      ewMinutes = Double.parseDouble(m1.group("ewMinutes"));
      ewSeconds = Double.parseDouble(m1.group("ewSeconds"));

      ns = m1.group("ns");
      ew = m1.group("ew");

      format = LatitudeLongitudeFormat.DMS;
    } else {
      Matcher m2 = getPatternFromFormat(LatitudeLongitudeFormat.COMPASS).matcher(identifier);
      if (!m2.matches()) {
        return null;
      }

      try {
        nsDegrees = Double.parseDouble(m2.group("nsDegrees"));
        nsMinutes = Double.parseDouble(m2.group("nsMinutes"));
        nsSeconds = Double.parseDouble(m2.group("nsSeconds"));
      } catch (Exception e) {
        nsDegrees = Double.parseDouble(m2.group("nsDegrees2"));
        nsMinutes = Double.parseDouble(m2.group("nsMinutes2"));
        nsSeconds = Double.parseDouble(m2.group("nsSeconds2"));
      }

      try {
        ewDegrees = Double.parseDouble(m2.group("ewDegrees"));
        ewMinutes = Double.parseDouble(m2.group("ewMinutes"));
        ewSeconds = Double.parseDouble(m2.group("ewSeconds"));
      } catch (Exception e) {
        ewDegrees = Double.parseDouble(m2.group("ewDegrees2"));
        ewMinutes = Double.parseDouble(m2.group("ewMinutes2"));
        ewSeconds = Double.parseDouble(m2.group("ewSeconds2"));
      }

      ns = m2.group("ns");
      ew = m2.group("ew");
    }

    latitude = GeoUtils.degreesToDecimal(nsDegrees, nsMinutes, nsSeconds);
    longitude = GeoUtils.degreesToDecimal(ewDegrees, ewMinutes, ewSeconds);

    if (ns.equals("S")) {
      latitude = -latitude;
    }
    if (ew.equals("W")) {
      longitude = -longitude;
    }

    return new LatitudeLongitude(latitude, longitude, format);
  }

  @Override
  protected Collection<Pattern> getPatterns() {
    return coordinatePatterns;
  }

  @Override
  protected Collection<String> getAppropriateNames() {
    return Arrays.asList(appropriateNames);
  }
}
