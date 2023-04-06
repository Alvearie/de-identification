/*
 * © Merative US L.P. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import com.ibm.whc.deid.models.ValueClass;
import com.ibm.whc.deid.providers.ProviderType;
import java.util.Collection;

/**
 * The interface Identifier.
 *
 */
public interface Identifier {
  
  /**
   * Gets type.
   *
   * @return the type
   */
  ProviderType getType();

  /**
   * Is of this type boolean.
   *
   * @param data the data
   * @return the boolean
   */
  boolean isOfThisType(String data);

  /**
   * Is appropriate name boolean.
   *
   * @param fieldName the field name
   * @return the boolean
   */
  boolean isAppropriateName(String fieldName);

  /**
   * Gets description.
   *
   * @return the description
   */
  String getDescription();

  /**
   * Gets value class.
   *
   * @return the value class
   */
  ValueClass getValueClass();

  /**
   * Gets linked types.
   *
   * @return the linked types
   */
  Collection<ProviderType> getLinkedTypes();

  /**
   * Gets priority.
   *
   * @return the priority
   */
  int getPriority();

  default int getMinimumCharacterRequirements() {
    return CharacterRequirements.NONE | CharacterRequirements.DIGIT | CharacterRequirements.ALPHA
        | CharacterRequirements.AT | CharacterRequirements.DOT | CharacterRequirements.SPACE
        | CharacterRequirements.DASH | CharacterRequirements.DOUBLEDOT;
  }

  default int getMinimumLength() {
    return 0;
  }

  default int getMaximumLength() {
    return Integer.MAX_VALUE;
  }

  default boolean isPOSIndependent() {
    return false;
  }
}
