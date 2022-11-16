/*
 * © Merative US L.P. 2016, 2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import java.text.Normalizer;
import java.util.Arrays;
import java.util.Collection;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import com.ibm.whc.deid.models.Address;
import com.ibm.whc.deid.models.ValueClass;
import com.ibm.whc.deid.providers.ProviderType;

/**
 * The type Address identifier.
 *
 */
public class AddressIdentifier extends AbstractIdentifier {

  private static final long serialVersionUID = -7030628863543238721L;

  private static final Pattern[] poBoxPatterns =
      {Pattern.compile("(PO|P.O.) BOX (?<poboxnumber>\\d+){1,1}")};
  private final String[] appropriateNames = {"Address"};
  /** The Road type pattern. */
  Pattern roadTypePattern = Pattern
      .compile(
          "\\s+(?<roadtype>STREET|ST\\.|ST|DRIVE|DR\\.|DR|BOULEVARD|BLVD\\.|BLVD|COURT|CT\\.|CT|"
              + "ROAD|RD\\.|RD|AVENUE|AVE\\.|AVE|LANE|LN\\.|LN)(\\s*,|\\s*\\z|\\s+)");

  Pattern firstPartPattern =
      Pattern.compile("^(?<number>\\d+){0,1}\\s*(?<street>(([\\w|\\d]+)\\s*)+)");
  Pattern secondPartPattern = Pattern.compile(
      ",\\s*(?<cityorstate>(([\\p{L}\\u0327\\u0331.()'‘’/-]+)[\\s]+)+)(?<postal>([A-Z]*\\d+[A-Z]*(\\s+[A-Z]*\\d+[A-Z]*)*\\s*)){0,1}(,\\s+(?<country>\\w+(\\s+\\w+)*)\\s*){0,1}",
      Pattern.UNICODE_CHARACTER_CLASS);

  /**
   * Remove diacritical marks string.
   *
   * @param string the string
   * @return the string
   */
  public static String removeDiacriticalMarks(String string) {
    return Normalizer.normalize(string, Normalizer.Form.NFD)
        .replaceAll("\\p{InCombiningDiacriticalMarks}+", "");
  }

  @Override
  public ProviderType getType() {
    return ProviderType.ADDRESS;
  }

  /**
   * If the given address string starts with a post office box number, update the given address
   * accordingly and return the offset of the last character matched.
   * 
   * @param key the address string to examine
   * 
   * @param address the address that will be updated in place if a post office box number is found
   * 
   * @return the offset of the last character matched in the address by the post office box search
   *         or -1 if no post office box number was found.
   */
  private int tryParsePOBOX(String key, Address address) {
    if (key.startsWith("PO ") || key.startsWith("P.O. ")) {
      for (Pattern p : poBoxPatterns) {
        Matcher m = p.matcher(key);
        if (m.find()) {
          String pobox = m.group("poboxnumber");
          address.setPoBox(true);
          address.setPoBoxNumber(pobox);
          return m.end();
        }
      }
    }
    return -1;
  }

  /**
   * Parse address address.
   *
   * @param data the data
   * @return the address
   */
  public Address parseAddress(String data) {
    // String key = removeDiacriticalMarks(data.trim()).toUpperCase();
    String key = data.toUpperCase();

    Address address = new Address();

    int secondPartStart = tryParsePOBOX(key, address);
    if (secondPartStart == -1) {
      Matcher roadtypeMatch = roadTypePattern.matcher(key);

      int roadtypeMatchOffset = -1;
      int roadtypeMatchEnd = -1;
      String roadType = null;
      int startRoadTypeSearchIndex = 0;

      while (roadtypeMatch.find(startRoadTypeSearchIndex)) {
        roadtypeMatchOffset = roadtypeMatch.start();
        roadtypeMatchEnd = roadtypeMatch.end();
        roadType = roadtypeMatch.group("roadtype").trim();
        // if EOL, stop
        if (roadtypeMatchEnd == key.length()) {
          break;
        }
        // if ends with comma, backup next match to start at comma and
        // stop looking for road type names
        if (key.charAt(roadtypeMatchEnd - 1) == ',') {
          roadtypeMatchEnd--;
          break;
        }
        // otherwise, backup next match one char to start at whitespace
        roadtypeMatchEnd--;
        startRoadTypeSearchIndex = roadtypeMatchEnd;
      }

      if (roadtypeMatchOffset < 5) {
        return null;
      }

      Matcher firstPartMatch = firstPartPattern.matcher(key.substring(0, roadtypeMatchOffset));
      if (!firstPartMatch.find()) {
        return null;
      }

      String number = firstPartMatch.group("number");
      if (number == null) {
        number = "";
      }

      String street = firstPartMatch.group("street").trim();

      address.setRoadType(roadType);
      address.setNumber(number);
      address.setName(street);

      secondPartStart = roadtypeMatchEnd;
    }

    String cityOrState;
    String postal;
    String country;

    Matcher secondPartMatch = secondPartPattern.matcher(key.substring(secondPartStart));
    if (!secondPartMatch.matches()) {
      cityOrState = "";
      postal = "";
      country = "";
    } else {
      cityOrState = secondPartMatch.group("cityorstate").trim();

      postal = secondPartMatch.group("postal");
      if (postal == null) {
        postal = "";
      }

      country = secondPartMatch.group("country");
      if (country == null) {
        country = "";
      }
    }

    address.setCityOrState(cityOrState);
    address.setPostalCode(postal.trim());
    address.setCountry(country.trim());

    return address;
  }

  @Override
  public boolean isOfThisType(String data) {
    return (parseAddress(data) != null);
  }

  @Override
  public String getDescription() {
    return "Address identification of the most common formats like \"200 Main Street, NY, USA\", \"PO BOX 123\" etc.";
  }

  @Override
  public ValueClass getValueClass() {
    return ValueClass.LOCATION;
  }

  @Override
  protected Collection<String> getAppropriateNames() {
    return Arrays.asList(this.appropriateNames);
  }
}
