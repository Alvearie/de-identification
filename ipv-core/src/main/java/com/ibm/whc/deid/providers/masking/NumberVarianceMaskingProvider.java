/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import com.ibm.whc.deid.shared.pojo.config.masking.NumberVarianceMaskingProviderConfig;
import com.ibm.whc.deid.util.RandomGenerators;

public class NumberVarianceMaskingProvider extends AbstractMaskingProvider {

  private static final long serialVersionUID = -7031813954318819006L;

  private final double limitDown;
  private final double limitUp;

  private final boolean augmentMask;
  private final double augmentLowerBound;
  private final double augmentUpperBound;
  private final boolean resultWithPrecision;
  private final int precisionDigits;

  public NumberVarianceMaskingProvider() {
    this(new NumberVarianceMaskingProviderConfig());
  }

  public NumberVarianceMaskingProvider(NumberVarianceMaskingProviderConfig configuration) {
    super(configuration);
    this.limitDown = configuration.getMaskLimitDown();
    this.limitUp = configuration.getMaskLimitUp();
    this.augmentMask = configuration.isAugmentMask();
    this.augmentLowerBound = configuration.getAugmentLowerBound();
    this.augmentUpperBound = configuration.getAugmentUpperBound();
    this.resultWithPrecision = configuration.isResultWithPrecision();
    this.precisionDigits = configuration.getPrecisionDigits();
  }

  @Override
  public String mask(String identifier) {
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    double number;

    try {
      number = Double.parseDouble(identifier);
    } catch (NumberFormatException e) {
      // For this provider, we do not return random value
      return applyUnexpectedValueHandling(identifier, null);
    }

    String numberAsString;

    if (augmentMask) {
      // Ensure proper range boundaries
      if ((augmentLowerBound > augmentUpperBound)
          || (augmentLowerBound == augmentUpperBound && !resultWithPrecision)) {
        throw new IllegalArgumentException(Double.toString(augmentLowerBound) + " "
            + Double.toString(augmentUpperBound) + " " + Boolean.toString(resultWithPrecision));
      }

      if (resultWithPrecision) {
        if (number == (int) number) {
          number += RandomGenerators.randomWithinRange((int) augmentLowerBound, 0, 0, 0,
              (int) (augmentUpperBound - augmentLowerBound));
        } else {
          number += RandomGenerators.randomWithinRangeWithPrecision(augmentLowerBound, 0, 0, 0,
              augmentUpperBound - augmentLowerBound);
        }
        if (precisionDigits < 0) {
          numberAsString = Double.toString(number);
        } else {
          numberAsString = String.format("%." + precisionDigits + "f", Double.valueOf(number));
        }
      } else {
        number += RandomGenerators.randomWithinRange((int) augmentLowerBound, 0, 0, 0,
            (int) (augmentUpperBound - augmentLowerBound));
        number = (int) number;
        numberAsString = Double.toString(number);
      }

      return numberAsString;
    }

    if (resultWithPrecision) {
      if (number == (int) number) {
        int percentage = RandomGenerators.randomWithinRange(0, (int) limitDown, (int) limitUp);
        number += number * percentage / 100.0;
      } else {
        double percentage = RandomGenerators.randomWithinRangeWithPrecision(0, limitDown, limitUp);
        number += number * percentage / 100.0;
      }
      if (precisionDigits < 0) {
        numberAsString = Double.toString(number);
      } else {
        numberAsString = String.format("%." + precisionDigits + "f", Double.valueOf(number));
      }
    } else {
      int percentage = RandomGenerators.randomWithinRange(0, (int) limitDown, (int) limitUp);
      number += number * percentage / 100.0;
      numberAsString = Double.toString(number);
    }

    return numberAsString;
  }
}
