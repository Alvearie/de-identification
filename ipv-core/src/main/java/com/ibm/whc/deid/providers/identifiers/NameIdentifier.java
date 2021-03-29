/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import java.util.Arrays;
import java.util.Collection;

import com.ibm.whc.deid.models.ValueClass;
import com.ibm.whc.deid.providers.ProviderType;
import com.ibm.whc.deid.util.NamesManager.NameManager;
import com.ibm.whc.deid.util.NumberUtils;
import com.ibm.whc.deid.util.Tuple;

/** The type Name identifier. */
public class NameIdentifier extends AbstractIdentifier implements IdentifierWithOffset {
  /** */
  private static final long serialVersionUID = -3461223001285905204L;

	private NameManager nameNanager;
  private static final String[] appropriateNames = {"Name", "Surname"};

	protected volatile boolean initialized = false;
	
	protected String tenantId;
	protected String localizationProperty;

	public NameIdentifier(String tenantId, String localizationProperty) {
		this.tenantId = tenantId;
		this.localizationProperty = localizationProperty;
	}

  @Override
  public ProviderType getType() {
    return ProviderType.NAME;
  }

	protected NameManager getManager() {
		if (!initialized) {
			nameNanager = new NameManager(tenantId, localizationProperty);
			initialized = true;
		}
		return nameNanager;
	}

  @Override
  public boolean isOfThisType(String data) {
    final String[] parts = data.split("\\s");

    boolean hasSurname = false;
    boolean hasName = false;

    for (String candidate : parts) {
      // System.out.println(candidate);
      if (candidate.length() > 3) {

        if (candidate.endsWith(",") || candidate.endsWith(".")) //
          candidate = candidate.substring(0, candidate.length() - 2);

        if (candidate.length() < 3)
          continue; // skip initials & co.

        candidate = candidate.toUpperCase();

				if (getManager().isLastName(candidate))
          hasSurname = true;
				else if (getManager().isFirstName(candidate))
          hasName = true;
        else {
          return false; // something does not match, maybe avenue or
          // so?
        }
      } else {
        // check if number
        try {
          int v = Integer.parseInt(candidate);

          if (v != 0)
            return false;
        } catch (NumberFormatException e) {
          // TMCH
        }
      }
    }

    return hasSurname || hasName; // need to better understand the possible
    // combinations
  }

  @Override
  public String getDescription() {
    return "Name identification based on popular lists";
  }

  @Override
  public ValueClass getValueClass() {
    return ValueClass.TEXT;
  }

  @Override
  protected Collection<String> getAppropriateNames() {
    return Arrays.asList(appropriateNames);
  }

  @Override
  public Tuple<Boolean, Tuple<Integer, Integer>> isOfThisTypeWithOffset(String data) {
    final String[] parts = data.split("\\s");

    boolean hasSurname = false;
    boolean hasName = false;

    if (NumberUtils.countDigits(data) > 0) {
      return new Tuple<>(false, null);
    }

    int offset = 0;
    int depth = data.length();

    for (int i = 0; i < parts.length; i++) {
      String candidate = parts[i];

      if (candidate.length() >= 1 && !Character.isUpperCase(candidate.charAt(0))) {
        return new Tuple<>(false, null);
      }

      if (candidate.length() >= 3) {

        if (candidate.endsWith(",") || candidate.endsWith(".")) {
          candidate = candidate.substring(0, candidate.length() - 1);

          if (i == (parts.length - 1)) {
            depth -= 1;
          }
        }

        if (candidate.length() < 3)
          continue; // skip initials & co.

        candidate = candidate.toUpperCase();

				if (getManager().isLastName(candidate))
          hasSurname = true;
				else if (getManager().isFirstName(candidate))
          hasName = true;
        else {
          return new Tuple<>(false, null); // something does not match, maybe avenue or so?
        }
      }
    }

    boolean result = hasSurname || hasName; // need to better understand the possible combinations

    if (result) {
      return new Tuple<>(true, new Tuple<>(offset, depth));
    }

    return new Tuple<>(false, null);
  }
}
