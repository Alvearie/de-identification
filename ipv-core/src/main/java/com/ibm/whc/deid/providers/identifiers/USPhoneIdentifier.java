/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import com.ibm.whc.deid.models.ValueClass;
import com.ibm.whc.deid.providers.ProviderType;
import com.ibm.whc.deid.util.Tuple;

public class USPhoneIdentifier extends AbstractIdentifier implements IdentifierWithOffset {
  /** */
  private static final long serialVersionUID = -366339981495614697L;

  @Override
  public ProviderType getType() {
    return ProviderType.PHONE;
  }

  @Override
  public boolean isOfThisType(String data) {
    return isOfThisTypeWithOffset(data).getFirst();
  }

  @Override
  public String getDescription() {
    return "US specific phone/fax/beeper identifier";
  }

  @Override
  public ValueClass getValueClass() {
    return ValueClass.TEXT;
  }

  private static final String PHONE_PATTERN =
      "#?\\s*((?:(?:\\(\\s*\\d{3}\\s*\\)|(?:1-)?\\d{3}-)\\s*)?\\d{3}-\\d{4})";
  private static final String PHONE_REG_EX = "\\s*(?::|No\\.\\s*:?|H|M)?\\s*";

  private static final List<Pattern> patterns =
      Arrays.asList(Pattern.compile(PHONE_PATTERN, Pattern.CASE_INSENSITIVE),
          // Pgr|Ph|Fax|Phone|Contact|Mobile(?:\s*No.)?
          Pattern.compile("Pgr" + PHONE_REG_EX + PHONE_PATTERN, Pattern.CASE_INSENSITIVE),
          Pattern.compile("Tel(?:ephone)?" + PHONE_REG_EX + PHONE_PATTERN,
              Pattern.CASE_INSENSITIVE),
          Pattern.compile("cell" + PHONE_REG_EX + PHONE_PATTERN, Pattern.CASE_INSENSITIVE),
          Pattern.compile("Fax" + PHONE_REG_EX + PHONE_PATTERN, Pattern.CASE_INSENSITIVE),
          Pattern.compile("Contact" + PHONE_REG_EX + PHONE_PATTERN, Pattern.CASE_INSENSITIVE),
          Pattern.compile("Mobile" + PHONE_REG_EX + PHONE_PATTERN, Pattern.CASE_INSENSITIVE),
          Pattern.compile("Home" + PHONE_REG_EX + PHONE_PATTERN, Pattern.CASE_INSENSITIVE),
          Pattern.compile("Work" + PHONE_REG_EX + PHONE_PATTERN, Pattern.CASE_INSENSITIVE),
          Pattern.compile("Ph(?:one)?" + PHONE_REG_EX + PHONE_PATTERN, Pattern.CASE_INSENSITIVE));

  @Override
  public Tuple<Boolean, Tuple<Integer, Integer>> isOfThisTypeWithOffset(String data) {
    for (Pattern pattern : patterns) {
      Matcher matcher = pattern.matcher(data);
      if (matcher.matches()) {
        return new Tuple<>(true, new Tuple<>(matcher.start(1), matcher.end(1) - matcher.start(1)));
      }
    }

    return new Tuple<>(false, null);
    /*
     * return patterns.parallelStream().map( pattern -> pattern.matcher(data) ).filter(
     * Matcher::matches ).map( matcher -> new Tuple<>(true, new Tuple<>(matcher.start(1),
     * matcher.end(1) - matcher.start(1))) ).reduce( (a, b) -> a ).orElse(new Tuple<>(false, null));
     */
  }

  @Override
  public int getMinimumCharacterRequirements() {
    return CharacterRequirements.DIGIT;
  }

  @Override
  public int getMinimumLength() {
    return 8;
  }

  @Override
  public int getMaximumLength() {
    return Integer.MAX_VALUE;
  }
}
