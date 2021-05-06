/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import com.ibm.whc.deid.models.LatitudeLongitude;
import com.ibm.whc.deid.providers.identifiers.LatitudeLongitudeIdentifier;
import com.ibm.whc.deid.shared.pojo.config.masking.LatitudeLongitudeMaskingProviderConfig;
import com.ibm.whc.deid.util.RandomGenerators;

/**
 * The type Latitude longitude masking provider.
 *
 */
public class LatitudeLongitudeMaskingProvider extends AbstractMaskingProvider {

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

  private final int maximumOffsetRadius;
  private final int minimumOffsetRadius;

  private final LatitudeLongitudeIdentifier latitudeLongitudeIdentifier =
      new LatitudeLongitudeIdentifier();

  /**
   * Instantiates a new Latitude longitude masking provider.
   *
   * @param configuration the configuration
   */
  public LatitudeLongitudeMaskingProvider(LatitudeLongitudeMaskingProviderConfig configuration) {
    super(configuration);

    this.randomWithinCircle = configuration.isMaskRandomWithinCircle();
    this.donutMasking = configuration.isMaskDonutMasking();
    this.fixedRadiusRandomDirection = configuration.isMaskFixedRadiusRandomDirection();

    this.minimumOffsetRadius = configuration.getOffsetMinimumRadius();
    this.maximumOffsetRadius = configuration.getOffsetMaximumRadius();

    if (this.maximumOffsetRadius <= LatitudeLongitudeMaskingProviderConfig.MINIMUM_OFFSET) {
      throw new IllegalArgumentException(
          "invalid maximum offset radius:" + this.maximumOffsetRadius);
    }

    if (this.minimumOffsetRadius <= LatitudeLongitudeMaskingProviderConfig.MINIMUM_OFFSET) {
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

    } else { // this.fixedRadiusRandomDirection
      randomLatLon = RandomGenerators.generateRandomCoordinateRandomDirection(latitudeLongitude,
          this.maximumOffsetRadius);
    }
    return randomLatLon.toString(latitudeLongitude.getFormat());
  }

  @Override
  public String mask(String identifier) {
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    LatitudeLongitude latitudeLongitude = latitudeLongitudeIdentifier.parseCoordinate(identifier);

    if (this.randomWithinCircle || this.donutMasking || this.fixedRadiusRandomDirection) {
      if (latitudeLongitude == null) {
        return applyUnexpectedValueHandling(identifier,
            () -> RandomGenerators.generateRandomCoordinate().toString());
      }
      return mask(latitudeLongitude);
    }

    LatitudeLongitude randomLatLon = RandomGenerators.generateRandomCoordinate();
    return latitudeLongitude == null ? randomLatLon.toString()
        : randomLatLon.toString(latitudeLongitude.getFormat());
  }
}
