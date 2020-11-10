/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import com.ibm.whc.deid.models.ValueClass;
import com.ibm.whc.deid.providers.ProviderType;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.regex.Pattern;

/**
 * The type Patient id identifier.
 *
 */
public class PatientIDIdentifier extends AbstractRegexBasedIdentifier {
  /** */
  private static final long serialVersionUID = -5409322816237544832L;

  private static final String[] appropriateNames = {"Patient ID", "PatientID"};
  private final Collection<Pattern> patientIDPatterns =
      new ArrayList<>(Arrays.asList(Pattern.compile("^\\d{3}-\\d{3}-\\d{3}-\\d{3}$")));

  @Override
  public ProviderType getType() {
    return ProviderType.EMAIL;
  }

  @Override
  public String getDescription() {
    return "Patient ID identification";
  }

  @Override
  public ValueClass getValueClass() {
    return ValueClass.TEXT;
  }

  @Override
  protected Collection<Pattern> getPatterns() {
    return patientIDPatterns;
  }

  @Override
  protected Collection<String> getAppropriateNames() {
    return Arrays.asList(appropriateNames);
  }
}
