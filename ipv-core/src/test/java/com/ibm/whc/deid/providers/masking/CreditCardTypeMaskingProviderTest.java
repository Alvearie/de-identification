/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import com.ibm.whc.deid.models.OriginalMaskedValuePair;
import com.ibm.whc.deid.models.ValueClass;
import com.ibm.whc.deid.providers.ProviderType;
import com.ibm.whc.deid.providers.identifiers.CreditCardTypeIdentifier;
import com.ibm.whc.deid.schema.FieldRelationship;
import com.ibm.whc.deid.schema.RelationshipOperand;
import com.ibm.whc.deid.schema.RelationshipType;
import java.util.HashMap;
import java.util.Map;
import org.junit.Test;

public class CreditCardTypeMaskingProviderTest extends TestLogSetUp {
  @Test
  public void testMask() {
    CreditCardTypeMaskingProvider maskingProvider = new CreditCardTypeMaskingProvider();
    CreditCardTypeIdentifier identifier = new CreditCardTypeIdentifier();

    String originalValue = "VISA";

    int nonMatches = 0;
    for (int i = 0; i < 1000; i++) {
      String maskedValue = maskingProvider.mask(originalValue);
      assertTrue(identifier.isOfThisType(maskedValue));
      if (!maskedValue.equals(originalValue)) {
        nonMatches++;
      }
    }

    assertTrue(nonMatches > 0);
  }

  @Test
  public void testCompoundMask() throws Exception {
    CreditCardTypeMaskingProvider maskingProvider = new CreditCardTypeMaskingProvider();

    String originalCCType = "VISA";

    Map<String, OriginalMaskedValuePair> maskedValues = new HashMap<>();
    maskedValues.put("cc", new OriginalMaskedValuePair("41223333333312345", "5523527012345678"));

    FieldRelationship fieldRelationship =
        new FieldRelationship(ValueClass.TEXT, RelationshipType.LINKED, "field0",
            new RelationshipOperand[] {new RelationshipOperand("cc", ProviderType.CREDIT_CARD)});

    for (int i = 0; i < 1000; i++) {
      String maskedCCType =
          maskingProvider.mask(originalCCType, "field0", fieldRelationship, maskedValues);
      assertEquals("Mastercard".toUpperCase(), maskedCCType.toUpperCase());
    }
  }
}
