/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import java.security.SecureRandom;
import com.ibm.whc.deid.configuration.MaskingConfiguration;
import com.ibm.whc.deid.models.LatitudeLongitude;
import com.ibm.whc.deid.providers.identifiers.LatitudeLongitudeIdentifier;
import com.ibm.whc.deid.shared.pojo.config.masking.LatitudeLongitudeMaskingProviderConfig;
import com.ibm.whc.deid.util.RandomGenerators;

/**
 * The type Latitude longitude masking provider.
 *
 */
public class LatitudeLongitudeMaskingProvider extends AbstractMaskingProvider {
  /** */
  private static final long serialVersionUID = -2645862297057567805L;

  private final boolean fixedRadiusRandomDirection;
  private final boolean donutMasking;
  private final boolean randomWithinCircle;

  // TODO: implement NRand and theta rand:
  // private final boolean nrand;
  // private final boolean thetaRand;

  // TODO : digit reduction:
  // private final boolean digitReduction
  // private final int digitsToReduct

  private static final int MINIMUM_OFFSET = 10;

  private final int maximumOffsetRadius;
  private final int minimumOffsetRadius;

  private final LatitudeLongitudeIdentifier latitudeLongitudeIdentifier =
      new LatitudeLongitudeIdentifier();

  private final int unspecifiedValueHandling;
  private final String unspecifiedValueReturnMessage;

  public LatitudeLongitudeMaskingProvider(MaskingConfiguration maskingConfiguration) {
    this(new SecureRandom(), maskingConfiguration);
  }

  /**
   * Instantiates a new Latitude longitude masking provider.
   *
   * @param configuration the configuration
   */
  public LatitudeLongitudeMaskingProvider(SecureRandom random, MaskingConfiguration configuration) {

    this.randomWithinCircle = configuration.getBooleanValue("latlon.mask.randomWithinCircle");
    this.donutMasking = configuration.getBooleanValue("latlon.mask.donutMasking");
    this.fixedRadiusRandomDirection =
        configuration.getBooleanValue("latlon.mask.fixedRadiusRandomDirection");

    this.minimumOffsetRadius = configuration.getIntValue("latlon.offset.minimumRadius");
    this.maximumOffsetRadius = configuration.getIntValue("latlon.offset.maximumRadius");

    this.unspecifiedValueHandling = configuration.getIntValue("unspecified.value.handling");
    this.unspecifiedValueReturnMessage =
        configuration.getStringValue("unspecified.value.returnMessage");

    if (this.maximumOffsetRadius <= MINIMUM_OFFSET) {
      throw new IllegalArgumentException(
          "invalid maximum offset radius:" + this.maximumOffsetRadius);
    }

    if (this.minimumOffsetRadius <= MINIMUM_OFFSET) {
      throw new IllegalArgumentException(
          "invalid minimum offset radius:" + this.minimumOffsetRadius);
    }
  }

  public LatitudeLongitudeMaskingProvider(LatitudeLongitudeMaskingProviderConfig configuration) {

    this.randomWithinCircle = configuration.isMaskRandomWithinCircle();
    this.donutMasking = configuration.isMaskDonutMasking();
    this.fixedRadiusRandomDirection = configuration.isMaskFixedRadiusRandomDirection();

    this.minimumOffsetRadius = configuration.getOffsetMinimumRadius();
    this.maximumOffsetRadius = configuration.getOffsetMaximumRadius();

    this.unspecifiedValueHandling = configuration.getUnspecifiedValueHandling();
    this.unspecifiedValueReturnMessage = configuration.getUnspecifiedValueReturnMessage();

    if (this.maximumOffsetRadius <= MINIMUM_OFFSET) {
      throw new IllegalArgumentException(
          "invalid maximum offset radius:" + this.maximumOffsetRadius);
    }

    if (this.minimumOffsetRadius <= MINIMUM_OFFSET) {
      throw new IllegalArgumentException(
          "invalid minimum offset radius:" + this.minimumOffsetRadius);
    }
  }

  /** Instantiates a new Latitude longitude masking provider. */
  public LatitudeLongitudeMaskingProvider() {
    this(new LatitudeLongitudeMaskingProviderConfig());
  }

  private String mask(LatitudeLongitude latitudeLongitude) {
    LatitudeLongitude randomLatLon;

    if (this.randomWithinCircle) {
      randomLatLon =
          RandomGenerators.generateRandomCoordinate(latitudeLongitude, this.maximumOffsetRadius);
    } else if (this.donutMasking) {
      randomLatLon = RandomGenerators.generateRandomCoordinate(latitudeLongitude,
          this.minimumOffsetRadius, this.maximumOffsetRadius);
    } else if (this.fixedRadiusRandomDirection) {
      randomLatLon = RandomGenerators.generateRandomCoordinateRandomDirection(latitudeLongitude,
          this.maximumOffsetRadius);
    } else {
      randomLatLon = RandomGenerators.generateRandomCoordinate();
    }
    randomLatLon.setFormat(latitudeLongitude.getFormat());

    return randomLatLon.toString();
  }

  @Override
  public String mask(String identifier) {
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    LatitudeLongitude latitudeLongitude = latitudeLongitudeIdentifier.parseCoordinate(identifier);
    if (latitudeLongitude == null) {
      debugFaultyInput("latitudeLongitude");
      if (unspecifiedValueHandling == 2) {
        return RandomGenerators.generateRandomCoordinate().toString();
      } else if (unspecifiedValueHandling == 3) {
        return unspecifiedValueReturnMessage;
      } else {
        return null;
      }
    }

    return mask(latitudeLongitude);
  }
}
