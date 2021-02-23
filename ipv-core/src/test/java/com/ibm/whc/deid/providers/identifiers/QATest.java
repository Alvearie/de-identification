/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import static org.junit.Assert.assertEquals;

import java.util.Collection;

import org.junit.Test;

import com.ibm.whc.deid.providers.ProviderType;
import com.ibm.whc.deid.util.localization.LocalizationManager;

public class QATest {
  private static final String charset =
      "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890!@#$%^&*()_+=-{}[]:;\"'\\?/<>,.~`";

	String tenantId = "TEST_TENANT";
  @Test
  public void testOneLetters() {

    Collection<Identifier> identifiers =
				IdentifierFactoryUtil.getIdentifierFactory().getAvailableIdentifiers(tenantId,
						LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES);

    for (int i = 0; i < charset.length(); i++) {
      String value = "" + charset.charAt(i);
      for (Identifier identifier : identifiers) {
        boolean match = identifier.isOfThisType(value);

        if (match == true && Character.isDigit(charset.charAt(i))
            && identifier.getType() == ProviderType.NUMERIC) {
          continue;
        }

        if (match == true) {
          System.out
              .println("value: " + value + " , identifier: " + identifier.getType().getName());
        }

        assertEquals(false, match);
      }
    }
  }

  @Test
  public void testTwoLetters() {

		Collection<Identifier> identifiers = IdentifierFactoryUtil.getIdentifierFactory()
				.getAvailableIdentifiers(tenantId, LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES);

    for (int i = 0; i < charset.length(); i++) {
      for (int j = 0; j < charset.length(); j++) {
        String value = "" + charset.charAt(i) + charset.charAt(j);

        for (Identifier identifier : identifiers) {
          boolean match = identifier.isOfThisType(value);

          if (match == true) {
            ProviderType providerType = identifier.getType();
            if (providerType == ProviderType.COUNTRY || providerType == ProviderType.NUMERIC
                || providerType == ProviderType.STATES_US) {
              continue;
            }

            if (value.equals("::") && providerType == ProviderType.IP_ADDRESS) {
              continue;
            }
          }

          if (match == true) {
            System.out
                .println("value: " + value + " , identifier: " + identifier.getType().getName());
          }

          assertEquals(false, match);
        }
      }
    }
  }
}
