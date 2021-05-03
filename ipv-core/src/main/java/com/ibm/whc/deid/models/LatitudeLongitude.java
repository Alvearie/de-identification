/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.models;

import java.io.Serializable;
import java.util.Objects;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.Messages;

/**
 * The latitude and longitude of a location on the globe.
 * 
 * <p>
 * This class is immutable and thread-safe.
 */
public class LatitudeLongitude implements Serializable {
  private static final long serialVersionUID = -7543687706274544761L;

  private final double latitude;
  private final double longitude;
  private final LatitudeLongitudeFormat format;

  /**
   * Instantiates a new object with the DECIMAL string representation format.
   *
   * @param latitude the latitude, -90.0 <= latitude <= 90.0
   * @param longitude the longitude, -180.0 < longitude < 180.0
   * 
   * @throws IllegalArgumentException if the latitude or longitude is out of range
   */
  public LatitudeLongitude(double latitude, double longitude) {
    this(latitude, longitude, LatitudeLongitudeFormat.DECIMAL);
  }

  /**
   * Instantiates a new object.
   *
   * @param latitude the latitude, -90.0 <= latitude <= 90.0
   * @param longitude the longitude, -180.0 < longitude < 180.0
   * @param format the default format used by the toString() method to represent this location -
   *        DECIMAL is used if <i>null</i> is given
   * 
   * @throws IllegalArgumentException if the latitude or longitude is out of range
   */
  public LatitudeLongitude(double latitude, double longitude, LatitudeLongitudeFormat format) {
    if (latitude < -90.0 || latitude > 90.0) {
      throw new IllegalArgumentException(
          Messages.getMessage(LogCodes.WPH1010E, Double.toString(latitude), "latitude"));
    }
    if (longitude < -180.0 || longitude > 180.0) {
      throw new IllegalArgumentException(
          Messages.getMessage(LogCodes.WPH1010E, Double.toString(longitude), "longitude"));
    }
    this.latitude = latitude;
    // -180 and +180 longitude are same meridian
    // use east in all cases to simplify equals() and hashCode()
    this.longitude = longitude == -180.0 ? 180.0 : longitude;
    this.format = format == null ? LatitudeLongitudeFormat.DECIMAL : format;
  }

  /**
   * Returns a new LatitudeLongitude with the DECIMAL string representation format or <i>null</i> if
   * either of the given latitude or longitude is null or all whitespace.
   *
   * @param latitude the latitude in string format, -90.0 <= latitude <= 90.0
   * @param longitude the longitude in string format, -180.0 < longitude < 180.0
   * 
   * @throws IllegalArgumentException if the latitude or longitude cannot be converted into a double
   *         value or is out of range
   */
  public static LatitudeLongitude buildLatitudeLongitude(String latitude, String longitude) {
    LatitudeLongitude value = null;
    if (latitude != null && longitude != null && !latitude.trim().isEmpty()
        && !longitude.trim().isEmpty()) {
      value = new LatitudeLongitude(convertToDouble(latitude, "latitude"),
          convertToDouble(longitude, "longitude"), LatitudeLongitudeFormat.DECIMAL);
    }
    return value;
  }

  private static double convertToDouble(String value, String component) {
    try {
      return Double.parseDouble(value);
    } catch (RuntimeException e) {
      throw new IllegalArgumentException(
          Messages.getMessage(LogCodes.WPH1010E, String.valueOf(value), component));
    }
  }

  /**
   * Retrieves the default format used by the toString() method to represent this location.
   *
   * @return the format
   */
  public LatitudeLongitudeFormat getFormat() {
    return this.format;
  }

  /**
   * Gets latitude.
   *
   * @return the latitude
   */
  public double getLatitude() {
    return this.latitude;
  }

  /**
   * Gets longitude.
   *
   * @return the longitude
   */
  public double getLongitude() {
    return this.longitude;
  }

  @Override
  public boolean equals(Object obj) {
    if (obj == null || getClass() != obj.getClass()) {
      return false;
    }
    LatitudeLongitude latlon = (LatitudeLongitude) obj;
    return latlon.getLatitude() == latitude && latlon.getLongitude() == longitude;
  }

  @Override
  public String toString() {
    return toString(format);
  }

  public String toString(LatitudeLongitudeFormat format) {
    if (format == LatitudeLongitudeFormat.DECIMAL) {
      return String.format("%f,%f", getLatitude(), getLongitude());
      /*
       * StringBuilder builder = new StringBuilder(); builder.append(getLatitude());
       * builder.append(","); builder.append(getLongitude()); return builder.toString();
       */
    }

    String ns = "N";
    String ew = "E";

    Double latitude = Double.valueOf(this.latitude);
    if (latitude < 0) {
      ns = "S";
      latitude = -latitude;
    }

    Double longitude = Double.valueOf(this.longitude);
    if (longitude < 0) {
      ew = "W";
      longitude = -longitude;
    }

    int nsDegrees = latitude.intValue();
    int nsMinutes = (int) ((latitude - nsDegrees) * 60);
    Double nsSeconds = (latitude - nsDegrees - nsMinutes / 60.0) * 3600;
    int ewDegrees = longitude.intValue();
    int ewMinutes = (int) ((longitude - ewDegrees) * 60);
    Double ewSeconds = (longitude - ewDegrees - ewMinutes / 60.0) * 3600;

    if (format == LatitudeLongitudeFormat.COMPASS) {
      return String.format("%s%02d.%02d.%02d %s%02d.%02d.%02d", ns, nsDegrees, nsMinutes,
          nsSeconds.intValue(), ew, ewDegrees, ewMinutes, ewSeconds.intValue());
    }

    return String.format("%02d:%02d'%f%s %02d:%02d'%f%s", nsDegrees, nsMinutes, nsSeconds, ns,
        ewDegrees, ewMinutes, ewSeconds, ew);
  }

  @Override
  public int hashCode() {
    return Objects.hash(Double.valueOf(latitude), Double.valueOf(longitude));
  }
}
