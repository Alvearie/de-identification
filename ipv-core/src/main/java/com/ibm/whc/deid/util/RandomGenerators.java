/*
 * © Merative US L.P. 2016,2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import java.security.SecureRandom;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoField;
import org.apache.commons.lang3.StringUtils;
import com.ibm.whc.deid.models.CreditCardType;
import com.ibm.whc.deid.models.LatitudeLongitude;
import com.ibm.whc.deid.models.LatitudeLongitudeFormat;
import com.ibm.whc.deid.providers.identifiers.IPAddressIdentifier;
import com.ibm.whc.deid.providers.masking.IPAddressMaskingProvider;

/** The type Random generators. */
public class RandomGenerators {
  private static final SecureRandom random = new SecureRandom();
  private static final SecureRandom randomRadian = new SecureRandom();
  private static final IPAddressIdentifier ipAddressIdentifier = new IPAddressIdentifier();
  private static final IPAddressMaskingProvider ipAddressMaskingProvider =
      new IPAddressMaskingProvider();
  private static final TLDManager tldManager = TLDManager.instance();

  private static final char[] alphaDigitSubset =
      "0123456789abcdefghijklmnopqrstuvwxyz".toCharArray();

  private RandomGenerators() {
    // no need to instantiate
  }

  /**
   * Luhn check digit int.
   *
   * @param body the body
   * @return the int
   */
  public static int luhnCheckDigit(String body) {
    boolean evenPosition = true;
    int sum = 0;

    for (int i = body.length() - 1; i >= 0; i--) {
      int n = body.charAt(i) - '0';
      if (evenPosition) {
        n *= 2;
        if (n > 9) {
          n = (n % 10) + 1;
        }
      }
      sum += n;
      evenPosition = !evenPosition;
    }

    int s = 10 - sum % 10;
    return (s % 10 == 0) ? 0 : s;
  }

  /**
   * Generate random credit card string.
   *
   * @return the string
   */
  public static String generateRandomCreditCard(CreditCardTypeManager creditCardTypeManager) {
    CreditCardType creditCard = creditCardTypeManager.getRandomValue();

    String[] prefixes = creditCard.getPrefixes();
    String randomCC = prefixes[random.nextInt(prefixes.length)];
    for (int i = randomCC.length(); i < 6; i++) {
      randomCC += randomDigit();
    }

    int length = creditCard.getMinimumLength();

    for (int i = 6; i < (length - 1); i++) {
      randomCC += randomDigit();
    }

    randomCC += (char) ('0' + luhnCheckDigit(randomCC));

    return randomCC;
  }

  /**
   * Random digit char.
   *
   * @return the char
   */
  public static char randomDigit() {
    return (char) ('0' + random.nextInt(10));
  }

  /**
   * Random within range int.
   *
   * @param base the base
   * @param rangeDown the range down
   * @param rangeUp the range up
   * @return the int
   */
  public static int randomWithinRange(int base, int rangeDown, int rangeUp) {
    return randomWithinRange(base, 0, rangeDown, 0, rangeUp);
  }

  /**
   * Random within range double.
   *
   * @param base the base
   * @param rangeDown the range down
   * @param rangeUp the range up
   * @return the double
   */
  public static double randomWithinRangeWithPrecision(double base, double rangeDown,
      double rangeUp) {
    return randomWithinRangeWithPrecision(base, 0, rangeDown, 0, rangeUp);
  }

  /**
   * Random within range int, with both min and max range.
   *
   * @param base the base
   * @param rangeDownMin the range down minimum
   * @param rangeDownMax the range down maximum
   * @param rangeUpMin the range up minimum
   * @param rangeUpMax the range up maximum
   * @return the int
   */
  public static int randomWithinRange(int base, int rangeDownMin, int rangeDownMax, int rangeUpMin,
      int rangeUpMax) {
    // If both max ranges are 0, then nothing to do
    if (rangeDownMax == 0 && rangeUpMax == 0) {
      return base;
    }

    // If both min ranges are 0, then just use single range
    if (rangeDownMin == 0 && rangeUpMin == 0) {
      return (base - rangeDownMax) + random.nextInt(1 + rangeUpMax + rangeDownMax);
    }

    // Determine if using up range or down range
    boolean useUpRange = rangeUpMax > 0 && (rangeDownMax == 0 || random.nextBoolean());
    if (useUpRange) {
      return (base + rangeUpMin)
          + (rangeUpMax > rangeUpMin ? random.nextInt(1 + rangeUpMax - rangeUpMin) : 0);
    }
    return (base - rangeDownMin)
        - (rangeDownMax > rangeDownMin ? random.nextInt(1 + rangeDownMax - rangeDownMin) : 0);
  }

  /**
   * Random within range with precision.
   *
   * @param base the base
   * @param rangeDownMin the range down min
   * @param rangeDownMax the range down max
   * @param rangeUpMin the range up min
   * @param rangeUpMax the range up max
   * @return the double
   */
  public static double randomWithinRangeWithPrecision(double base, double rangeDownMin,
      double rangeDownMax, double rangeUpMin, double rangeUpMax) {
    // If both max ranges are 0, then nothing to do
    if (rangeDownMax == 0 && rangeUpMax == 0) {
      return base;
    }

    // If both min ranges are 0, then just use single range
    if (rangeDownMin == 0 && rangeUpMin == 0) {
      return (base - rangeDownMax) + (1 + rangeUpMax + rangeDownMax) * random.nextDouble();
    }

    // Determine if using up range or down range
    boolean useUpRange = rangeUpMax > 0 && (rangeDownMax == 0 || random.nextBoolean());
    if (useUpRange) {
      return (base + rangeUpMin)
          + (rangeUpMax > rangeUpMin ? (1 + rangeUpMax - rangeUpMin) * random.nextDouble() : 0);
    }
    return (base - rangeDownMin)
        - (rangeDownMax > rangeDownMin ? (1 + rangeDownMax - rangeDownMin) * random.nextDouble()
            : 0);
  }

  /**
   * Gets random tld.
   *
   * @return the random tld
   */
  public static String getRandomTLD() {
    return tldManager.getRandomTLD();
  }

  /**
   * Random username and domain string.
   *
   * @return the string
   */
  public static String randomUsernameAndDomain(int n) {
    int length = 0;
    if (n != -1) {
      length = n;
    } else {
      length = 5 + random.nextInt(3);
    }
    final int subsetLength = alphaDigitSubset.length;
    char[] rnd = new char[length];

    rnd[0] = (char) ('a' + random.nextInt(26));
    for (int i = 1; i < length; i++) {
      int idx = random.nextInt(subsetLength);
      rnd[i] = alphaDigitSubset[idx];
    }

    return new String(rnd);
  }

  /**
   * Random replacement string.
   *
   * @param identifier the identifier
   * @return the string
   */
  public static String randomReplacement(String identifier) {
    StringBuilder builder = new StringBuilder();

    for (int i = 0; i < identifier.length(); i++) {
      char c = identifier.charAt(i);

      if (Character.isDigit(c)) {
        builder.append(RandomGenerators.randomDigit());
      } else if (Character.isUpperCase(c)) {
        builder.append((char) ('A' + random.nextInt(25)));
      } else if (Character.isLowerCase(c)) {
        builder.append((char) ('a' + random.nextInt(25)));
      } else if (Character.isAlphabetic(c)) {
        builder.append((char) ('A' + random.nextInt(25)));
      } else {
        builder.append(c);
      }
    }

    return builder.toString();
  }

  /**
   * Generate random url string.
   *
   * @return the string
   */
  public static String generateRandomURL() {
    String host = RandomGenerators.randomUIDGenerator(10);
    String tld = getRandomTLD();

    StringBuilder builder = new StringBuilder("http://");
    builder.append(host);
    builder.append('.');
    builder.append(tld);
    return builder.toString();
  }

  /**
   * @return a random date before the current year
   */
  public static LocalDateTime generateRandomDate() {
    LocalDateTime now = LocalDateTime.now();
    int yearRange = now.get(ChronoField.YEAR) - 1900;
    int randomYear = random.nextInt(yearRange) + 1900;
    int randomMonth = random.nextInt(12) + 1;
    int dayRange;
    switch (randomMonth) {
      case 1:
      case 3:
      case 5:
      case 7:
      case 8:
      case 10:
      case 12:
        dayRange = 31;
        break;
      case 4:
      case 6:
      case 9:
      case 11:
        dayRange = 30;
        break;
      case 2:
        dayRange = 28;
        if (isLeapYear(randomYear)) {
          dayRange++;
        }
        break;
      default:
        // should never occur
        throw new IllegalArgumentException(String.valueOf(randomMonth));
    }
    LocalDateTime randomLocalDateTime =
        LocalDateTime.of(randomYear, randomMonth, random.nextInt(dayRange) + 1,
            random.nextInt(24), random.nextInt(60), random.nextInt(60));
    return randomLocalDateTime;
  }

  public static boolean isLeapYear(int year) {
    return year % 4 == 0 && (year % 100 != 0 || year % 400 == 0);
  }

  /**
   * Generate random datetime string.
   *
   * @param dateFormat the datetime format - it can be assumed the random datetime object will
   *        include time zone information
   * 
   * @return a random date before the current year in string format
   */
  public static String generateRandomDate(DateTimeFormatter dateFormat) {
    ZonedDateTime randomZonedDateTime = generateRandomDate().atZone(ZoneId.systemDefault());
    return dateFormat.format(randomZonedDateTime);
  }

  /**
   * Generate random digit sequence string.
   *
   * @param length the length
   * @return the string
   */
  public static String generateRandomDigitSequence(int length) {
    /* TODO missing tests */
    StringBuilder builder = new StringBuilder();

    for (int i = 0; i < length; i++) {
      builder.append(RandomGenerators.randomDigit());
    }

    return builder.toString();
  }

  public static LatitudeLongitude generateRandomCoordinateRandomDirection(
      LatitudeLongitude latitudeLongitude, int distance) {

    double radian = randomRadian.nextDouble() * 2.0 * Math.PI;
    double latitude = latitudeLongitude.getLatitude();
    double longitude = latitudeLongitude.getLongitude();

    return generateRandomCoordinateFromBearing(latitude, longitude, radian, distance);
  }

  public static LatitudeLongitude generateRandomCoordinateFromBearing(double latitude,
      double longitude, double radian, int distance) {

    double delta = distance / GeoUtils.getR();
    double theta = Math.toRadians(radian);

    double f1 = Math.toRadians(latitude);
    double l1 = Math.toRadians(longitude);

    double f2 = Math
        .asin(Math.sin(f1) * Math.cos(delta) + Math.cos(f1) * Math.sin(delta) * Math.cos(theta));

    double l2 = l1 + Math.atan2(Math.sin(theta) * Math.sin(delta) * Math.cos(f1),
        Math.cos(delta) - Math.sin(f1) * Math.sin(f2));
    // Normalize to -180..+180
    l2 = (l2 + 3 * Math.PI) % (2 * Math.PI) - Math.PI;

    return new LatitudeLongitude(Math.toDegrees(f2), Math.toDegrees(l2));
  }

  public static LatitudeLongitude generateRandomCoordinateRandomDirection(double latitude,
      double longitude, int distance) {

    double radian = randomRadian.nextDouble() * 2.0 * Math.PI;
    return generateRandomCoordinateFromBearing(latitude, longitude, radian, distance);
  }

  public static LatitudeLongitude generateRandomCoordinate(LatitudeLongitude latitudeLongitude,
      int minimumOffsetRadius, int maximumOffsetRadius) {
    return generateRandomCoordinate(latitudeLongitude.getLatitude(),
        latitudeLongitude.getLongitude(), minimumOffsetRadius, maximumOffsetRadius);
  }

  public static LatitudeLongitude generateRandomCoordinate(double latitude, double longitude,
      int minimumOffsetRadius, int maximumOffsetRadius) {

    while (true) {
      LatitudeLongitude latitudeLongitude =
          generateRandomCoordinate(latitude, longitude, maximumOffsetRadius);
      double distance = GeoUtils.latitudeLongitudeDistance(latitude, longitude,
          latitudeLongitude.getLatitude(), latitudeLongitude.getLongitude());

      if (distance >= minimumOffsetRadius) {
        return latitudeLongitude;
      }
    }
  }

  /**
   * Generate random coordinate latitude longitude.
   *
   * @param latitude the latitude
   * @param longitude the longitude
   * @param offsetRadius the offset radius
   * @return the latitude longitude
   */
  public static LatitudeLongitude generateRandomCoordinate(double latitude, double longitude,
      int offsetRadius) {
    return generateRandomCoordinate(latitude, longitude, offsetRadius,
        LatitudeLongitudeFormat.DECIMAL);
  }

  /**
   * Generate random coordinate latitude longitude.
   *
   * @param latitude the latitude
   * @param longitude the longitude
   * @param offsetRadius the offset radius
   * @param format the default format to use when writing the latitude-longitude value
   * @return the latitude longitude
   */
  public static LatitudeLongitude generateRandomCoordinate(double latitude, double longitude,
      int offsetRadius, LatitudeLongitudeFormat format) {

    double radiusInDegrees = offsetRadius / 111000f;

    double u = random.nextDouble();
    double v = random.nextDouble();
    double w = radiusInDegrees * Math.sqrt(u);

    double t = 2 * Math.PI * v;
    double x = w * Math.cos(t);
    double y = w * Math.sin(t);

    // Adjust the x-coordinate for the shrinking of the east-west distances
    double new_x = x / Math.cos(longitude);

    double foundLatitude = y + latitude;
    if (foundLatitude > 90.0) {
      double diff = foundLatitude - 90.0;
      foundLatitude = Math.abs(-90 + diff);
    }

    double foundLongitude = new_x + longitude;
    if (foundLongitude > 180) {
      double diff = foundLongitude - 180.0;
      foundLongitude = Math.abs(-180 + diff);
    }

    return new LatitudeLongitude(foundLatitude, foundLongitude, format);
  }

  /**
   * Generate random coordinate latitude longitude.
   *
   * @param latitudeLongitude the latitude longitude
   * @param offsetRadius the offset radius
   * @return the latitude longitude
   */
  public static LatitudeLongitude generateRandomCoordinate(LatitudeLongitude latitudeLongitude,
      int offsetRadius) {
    LatitudeLongitude randomLatitudeLongitude = generateRandomCoordinate(
        latitudeLongitude.getLatitude(), latitudeLongitude.getLongitude(), offsetRadius,
        latitudeLongitude.getFormat());
    return randomLatitudeLongitude;
  }

  /**
   * Generate random coordinate latitude longitude.
   *
   * @return the latitude longitude
   */
  public static LatitudeLongitude generateRandomCoordinate() {
    double latitude = random.nextInt(90);
    double longitude = random.nextInt(180);

    // don't switch from 0.0 to -0.0
    if (latitude != 0.0 && random.nextBoolean()) {
      latitude = -latitude;
    }
    if (longitude != 0.0 && random.nextBoolean()) {
      longitude = -longitude;
    }

    return new LatitudeLongitude(latitude, longitude, LatitudeLongitudeFormat.DECIMAL);
  }

  private static String generateRandomHost(String hostTemplate, int preserveSubdomains) {
    String[] domains = hostTemplate.split("\\.");
    int domainsLength = domains.length;

    // we preserve everything
    if (preserveSubdomains >= domainsLength || preserveSubdomains == -1) {
      return hostTemplate;
    }

    for (int i = 0; i < (domainsLength - preserveSubdomains); i++) {
      domains[i] = RandomGenerators.randomUIDGenerator(domains[i].length());
    }

    return StringUtils.join(domains, ".");
  }

  /**
   * Random hostname generator string.
   *
   * @param hostname the hostname
   * @param preserveDomains the preserve domains
   * @return the string
   */
  public static String randomHostnameGenerator(String hostname, int preserveDomains) {

    if (preserveDomains == -1) {
      return hostname;
    }
    int idx;
    int preserveSubdomains = Math.max(preserveDomains - 1, 0);

    if (ipAddressIdentifier.isIPv4(hostname)) {
      return ipAddressMaskingProvider.directMask(hostname, true);
    } else if (ipAddressIdentifier.isIPv6(hostname)) {
      return ipAddressMaskingProvider.directMask(hostname, false);
    }

    if (preserveDomains == 0) {
      StringBuilder builder = new StringBuilder(generateRandomHost(hostname, preserveSubdomains));
      builder.append('.');
      builder.append(tldManager.getRandomTLD());
      return builder.toString();
    }

    String tld = tldManager.getTLD(hostname);
    if (tld == null || (idx = hostname.indexOf(tld)) == 0) {
      return generateRandomHost(hostname, preserveDomains);
    }

    hostname = hostname.substring(0, idx - 1);

    StringBuilder builder = new StringBuilder(generateRandomHost(hostname, preserveSubdomains));
    builder.append('.');
    builder.append(tld);
    return builder.toString();
  }

  private static boolean contains(char[] array, char element) {
    for (char c : array) {
      if (c == element) {
        return true;
      }
    }

    return false;
  }

  /**
   * Random uid generator with inclusions string.
   *
   * @param length the length
   * @param subset the subset
   * @return the string
   */
  public static String randomUIDGeneratorWithInclusions(int length, char[] subset) {
    int subsetLength = subset.length;
    StringBuilder builder = new StringBuilder();
    for (int i = 0; i < length; i++) {
      int idx = random.nextInt(subsetLength);
      char nextRandom = subset[idx];
      builder.append(nextRandom);
    }
    return builder.toString();
  }

  /**
   * Random uid generator string.
   *
   * @param length the length
   * @param excludedCharacters the excluded characters
   * @return the string
   */
  public static String randomUIDGenerator(int length, char[] excludedCharacters) {
    StringBuilder builder = new StringBuilder();
    char nextRandom;

    for (int i = 0; i < length; i++) {
      if (random.nextBoolean()) {
        nextRandom = (char) ('a' + random.nextInt(26));
      } else {
        nextRandom = (char) ('0' + random.nextInt(10));
      }

      if (excludedCharacters != null) {
        if (contains(excludedCharacters, nextRandom)) {
          i--;
          continue;
        }
      }

      builder.append(nextRandom);
    }

    return builder.toString();
  }

  /**
   * Random uid generator string.
   *
   * @param length the length
   * @return the string
   */
  public static String randomUIDGenerator(int length) {
    return randomUIDGenerator(length, null);
  }
}
