/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import com.ibm.whc.deid.shared.pojo.config.masking.NumberVarianceMaskingProviderConfig;
import com.ibm.whc.deid.util.RandomGenerators;

public class NumberVarianceMaskingProvider extends AbstractMaskingProvider {
  /** */
  private static final long serialVersionUID = -7031813954318819006L;

  private final double limitDown;
  private final double limitUp;

  private final boolean augmentMask;
  private final double augmentLowerBound;
  private final double augmentUpperBound;
  private final boolean resultWithPrecision;
  private final int precisionDigits;
  private final int unspecifiedValueHandling;
  private final String unspecifiedValueReturnMessage;

	public NumberVarianceMaskingProvider() {
		this(new NumberVarianceMaskingProviderConfig());
	}

  public NumberVarianceMaskingProvider(NumberVarianceMaskingProviderConfig configuration) {
    this.limitDown = configuration.getMaskLimitDown();
    this.limitUp = configuration.getMaskLimitUp();
    this.augmentMask = configuration.isAugmentMask();
    this.augmentLowerBound = configuration.getAugmentLowerBound();
    this.augmentUpperBound = configuration.getAugmentUpperBound();
    this.resultWithPrecision = configuration.isResultWithPrecision();
    this.precisionDigits = configuration.getPrecisionDigits();
    this.unspecifiedValueHandling = configuration.getUnspecifiedValueHandling();
    this.unspecifiedValueReturnMessage = configuration.getUnspecifiedValueReturnMessage();
  }

  @Override
  public String mask(String identifier) {
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    Double number;

    try {
      number = Double.valueOf(identifier);
    } catch (NumberFormatException e) {
      // For this provider, we do not return random value
      debugFaultyInput("number");
      if (unspecifiedValueHandling == 3) {
        return unspecifiedValueReturnMessage;
      } else {
        return null;
      }
    }

    String numberAsString;

    if (augmentMask) {
      // Ensure proper range boundaries. Null return value indicates
      // masking failure.
      if ((augmentLowerBound > augmentUpperBound) || // results
      // java.lang.IllegalArgumentException:
      // bound must be
      // positive
          (augmentLowerBound == augmentUpperBound && !resultWithPrecision)) // results
        // in
        // interval=0
        // and
        // unmasked
        // identifier
        return null;

      if (resultWithPrecision) {
        if (number == (int) number.doubleValue()) {
          number += RandomGenerators.randomWithinRange((int) augmentLowerBound, 0, 0, 0,
              (int) (augmentUpperBound - augmentLowerBound));
        } else {
          number += RandomGenerators.randomWithinRangeWithPrecision(augmentLowerBound, 0, 0, 0,
              augmentUpperBound - augmentLowerBound);
        }

        if (precisionDigits == -1) {
          numberAsString = number.toString();
        } else {
          numberAsString = String.format("%." + precisionDigits + "f", number);
        }
      } else {
        number += RandomGenerators.randomWithinRange((int) augmentLowerBound, 0, 0, 0,
            (int) (augmentUpperBound - augmentLowerBound));
        number = (double) (int) number.doubleValue();
        numberAsString = number.toString();
      }

      return numberAsString;
    }

    if (resultWithPrecision) {
      if (number == (int) number.doubleValue()) {
        int percentage = RandomGenerators.randomWithinRange(0, (int) limitDown, (int) limitUp);
        number += number * percentage / 100.0;
      } else {
        double percentage = RandomGenerators.randomWithinRangeWithPrecision(0, limitDown, limitUp);
        number += number * percentage / 100.0;
      }

      if (precisionDigits == -1) {
        numberAsString = number.toString();
      } else {
        numberAsString = String.format("%." + precisionDigits + "f", number);
      }
    } else {
      int percentage = RandomGenerators.randomWithinRange(0, (int) limitDown, (int) limitUp);
      number += number * percentage / 100.0;
      numberAsString = number.toString();
    }

    return numberAsString;
  }
}
