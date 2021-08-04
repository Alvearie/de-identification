/*
 * (C) Copyright IBM Corp. 2016,2021
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
              + "ROAD|RD\\.|RD|AVENUE|AVE\\.|AVE|LANE|LN\\.|LN)\\s*(,|\\z)");

  /** The First part pattern. */
  Pattern firstPartPattern =
      Pattern.compile("^(?<number>\\d+){0,1}\\s*(?<street>(([\\w|\\d]+)\\s*)+)");
  /** The Second part pattern. */
  Pattern secondPartPattern = Pattern.compile(
      ",\\s*(?<cityorstate>(([a-zA-Z.’]+)[\\s]+)+)(?<postal>([A-Z]*\\d+[A-Z]*\\s*)+){0,1}(,\\s+(?<country>(\\w+\\s*)+)){0,1}");

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

  private Address tryParsePOBOX(String key) {
    if (key.startsWith("PO ") || key.startsWith("P.O. ")) {
      for (Pattern p : poBoxPatterns) {
        Matcher m = p.matcher(key);
        if (m.matches()) {
          String poboxnumber = m.group("poboxnumber");
          Address address = new Address();
          address.setPoBox(true);
          address.setPoBoxNumber(poboxnumber);
          return address;
        }
      }
    }

    return null;
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

    Address address = tryParsePOBOX(key);
    if (address != null) {
      return address;
    }

    Matcher roadtypeMatch = roadTypePattern.matcher(key);

    int roadtypeMatchOffset = -1;
    int roadtypeMatchEnd = -1;
    String roadType = null;

    while (roadtypeMatch.find()) {
      roadtypeMatchOffset = roadtypeMatch.start();
      roadtypeMatchEnd = roadtypeMatch.end();
      roadType = roadtypeMatch.group("roadtype").trim();
      // The comma was possibly included as the matching end delimiter when the
      // road type was matched. If so, add it back to the sequence as the "secondPart"
      // expects to start with a comma. Also, since the comma was encountered, stop
      // looking for more road type names.
      if (key.charAt(roadtypeMatchEnd - 1) == ',') {
        roadtypeMatchEnd--;
        break;
      }
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

    String cityOrState;
    String postal;
    String country;

    Matcher secondPartMatch = secondPartPattern.matcher(key.substring(roadtypeMatchEnd));
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

    address = new Address();

    address.setRoadType(roadType);
    address.setNumber(number);
    address.setName(street);
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
