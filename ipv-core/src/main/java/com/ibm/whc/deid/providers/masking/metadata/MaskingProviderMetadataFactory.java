/*
 * (C) Copyright Merative 2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.metadata;

import java.util.Locale;
import com.ibm.whc.deid.providers.masking.metadata.impl.ATCMaskingProviderMetadataBuilder;
import com.ibm.whc.deid.providers.masking.metadata.impl.AddressMaskingProviderMetadataBuilder;
import com.ibm.whc.deid.providers.masking.metadata.impl.BinningMaskingProviderMetadataBuilder;
import com.ibm.whc.deid.providers.masking.metadata.impl.CityMaskingProviderMetadataBuilder;
import com.ibm.whc.deid.providers.masking.metadata.impl.ConditionalMaskingProviderMetadataBuilder;
import com.ibm.whc.deid.providers.masking.metadata.impl.ContinentMaskingProviderMetadataBuilder;
import com.ibm.whc.deid.providers.masking.metadata.impl.CountryMaskingProviderMetadataBuilder;
import com.ibm.whc.deid.providers.masking.metadata.impl.CountyMaskingProviderMetadataBuilder;
import com.ibm.whc.deid.providers.masking.metadata.impl.CreditCardMaskingProviderMetadataBuilder;
import com.ibm.whc.deid.providers.masking.metadata.impl.DateDependencyMaskingProviderMetadataBuilder;
import com.ibm.whc.deid.providers.masking.metadata.impl.DateTimeConsistentShiftMaskingProviderMetadataBuilder;
import com.ibm.whc.deid.providers.masking.metadata.impl.DateTimeMaskingProviderMetadataBuilder;
import com.ibm.whc.deid.providers.masking.metadata.impl.EmailMaskingProviderMetadataBuilder;
import com.ibm.whc.deid.providers.masking.metadata.impl.FPEMaskingProviderMetadataBuilder;
import com.ibm.whc.deid.providers.masking.metadata.impl.GeneralizeMaskingProviderMetadataBuilder;
import com.ibm.whc.deid.providers.masking.metadata.impl.HashMaskingProviderMetadataBuilder;
import com.ibm.whc.deid.providers.masking.metadata.impl.HospitalMaskingProviderMetadataBuilder;
import com.ibm.whc.deid.providers.masking.metadata.impl.IBANMaskingProviderMetadataBuilder;
import com.ibm.whc.deid.providers.masking.metadata.impl.ICDV10MaskingProviderMetadataBuilder;
import com.ibm.whc.deid.providers.masking.metadata.impl.ICDV9MaskingProviderMetadataBuilder;
import com.ibm.whc.deid.providers.masking.metadata.impl.IMEIMaskingProviderMetadataBuilder;
import com.ibm.whc.deid.providers.masking.metadata.impl.IPAddressMaskingProviderMetadataBuilder;
import com.ibm.whc.deid.providers.masking.metadata.impl.LongitudeLatitudeMaskingProviderMetadataBuilder;
import com.ibm.whc.deid.providers.masking.metadata.impl.MACAddressMaskingProviderMetadataBuilder;
import com.ibm.whc.deid.providers.masking.metadata.impl.NameMaskingProviderMetadataBuilder;
import com.ibm.whc.deid.providers.masking.metadata.impl.NullMaskingProviderMetadataBuilder;
import com.ibm.whc.deid.providers.masking.metadata.impl.NumberVarianceMaskingProviderMetadataBuilder;
import com.ibm.whc.deid.providers.masking.metadata.impl.OccupationMaskingProviderMetadataBuilder;
import com.ibm.whc.deid.providers.masking.metadata.impl.PhoneMaskingProviderMetadataBuilder;
import com.ibm.whc.deid.providers.masking.metadata.impl.PseudonymMaskingProviderMetadataBuilder;
import com.ibm.whc.deid.providers.masking.metadata.impl.RedactMaskingProviderMetadataBuilder;
import com.ibm.whc.deid.providers.masking.metadata.impl.ReplaceMaskingProviderMetadataBuilder;
import com.ibm.whc.deid.providers.masking.metadata.impl.SSNUKMaskingProviderMetadataBuilder;
import com.ibm.whc.deid.providers.masking.metadata.impl.SSNUSMaskingProviderMetadataBuilder;
import com.ibm.whc.deid.providers.masking.metadata.impl.SwiftMaskingProviderMetadataBuilder;
import com.ibm.whc.deid.providers.masking.metadata.impl.URLMaskingProviderMetadataBuilder;
import com.ibm.whc.deid.providers.masking.metadata.impl.VINMaskingProviderMetadataBuilder;
import com.ibm.whc.deid.providers.masking.metadata.impl.ZipCodeMaskingProviderMetadataBuilder;
import com.ibm.whc.deid.providers.masking.metadata.model.MaskingProviderMetadataModel;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderType;

/**
 * Entry point for obtaining metadata describing masking providers.
 */
public abstract class MaskingProviderMetadataFactory {

  /**
   * Obtains metadata for a given masking provider.
   * 
   * @param provider the provider about which information is required
   * 
   * @param locale the preferred language in which the user-friendly portions of the information are
   *        to be returned. If the given language is not available, the best available substitute is
   *        used.
   * 
   * @return the available information in structured form
   */
  public static MaskingProviderMetadataModel getMaskingProviderMetadata(
      MaskingProviderType provider, Locale locale) {
    MaskingProviderMetadataBuilder builder = null;
    switch (provider) {
      case ADDRESS:
        builder = new AddressMaskingProviderMetadataBuilder();
        break;
      case ATC:
        builder = new ATCMaskingProviderMetadataBuilder();
        break;
      case BINNING:
        builder = new BinningMaskingProviderMetadataBuilder();
        break;
      case CITY:
        builder = new CityMaskingProviderMetadataBuilder();
        break;
      case CONDITIONAL:
        builder = new ConditionalMaskingProviderMetadataBuilder();
        break;
      case CONTINENT:
        builder = new ContinentMaskingProviderMetadataBuilder();
        break;
      case COUNTRY:
        builder = new CountryMaskingProviderMetadataBuilder();
        break;
      case COUNTY:
        builder = new CountyMaskingProviderMetadataBuilder();
        break;
      case CREDIT_CARD:
        builder = new CreditCardMaskingProviderMetadataBuilder();
        break;
      case DATEDEPENDENCY:
        builder = new DateDependencyMaskingProviderMetadataBuilder();
        break;
      case DATETIME:
        builder = new DateTimeMaskingProviderMetadataBuilder();
        break;
      case DATETIME_CONSISTENT_SHIFT:
        builder = new DateTimeConsistentShiftMaskingProviderMetadataBuilder();
        break;
      case EMAIL:
        builder = new EmailMaskingProviderMetadataBuilder();
        break;
      case FHIR_MORTALITY_DEPENDENCY:
        // TODO: reinstate or completely remove this provider
        // builder = new FHIRMortalityDependencyMaskingProviderMetadataBuilder();
        break;
      case FPE:
        builder = new FPEMaskingProviderMetadataBuilder();
        break;
      case GENDER:
        builder = new MaskingProviderMetadataBuilderNoCommon();
        break;
      case GENERALIZE:
        builder = new GeneralizeMaskingProviderMetadataBuilder();
        break;
      case GUID:
        builder = new MaskingProviderMetadataBuilderNoCommon();
        break;
      case HASH:
        builder = new HashMaskingProviderMetadataBuilder();
        break;
      case HOSPITAL:
        builder = new HospitalMaskingProviderMetadataBuilder();
        break;
      case IBAN:
        builder = new IBANMaskingProviderMetadataBuilder();
        break;
      case ICDV9:
        builder = new ICDV9MaskingProviderMetadataBuilder();
        break;
      case ICDV10:
        builder = new ICDV10MaskingProviderMetadataBuilder();
        break;
      case IMEI:
        builder = new IMEIMaskingProviderMetadataBuilder();
        break;
      case IP_ADDRESS:
        builder = new IPAddressMaskingProviderMetadataBuilder();
        break;
      case LATITUDE_LONGITUDE:
        builder = new LongitudeLatitudeMaskingProviderMetadataBuilder();
        break;
      case MAC_ADDRESS:
        builder = new MACAddressMaskingProviderMetadataBuilder();
        break;
      case MAINTAIN:
        builder = new MaskingProviderMetadataBuilderNoCommon();
        break;
      case MARITAL:
        builder = new MaskingProviderMetadataBuilderBase();
        break;
      case NAME:
        builder = new NameMaskingProviderMetadataBuilder();
        break;
      case NULL:
        builder = new NullMaskingProviderMetadataBuilder();
        break;
      case NUMBERVARIANCE:
        builder = new NumberVarianceMaskingProviderMetadataBuilder();
        break;
      case OCCUPATION:
        builder = new OccupationMaskingProviderMetadataBuilder();
        break;
      case PHONE:
        builder = new PhoneMaskingProviderMetadataBuilder();
        break;
      case PSEUDONYM:
        builder = new PseudonymMaskingProviderMetadataBuilder();
        break;
      case RACE:
        builder = new MaskingProviderMetadataBuilderBase();
        break;
      case RANDOM:
        builder = new MaskingProviderMetadataBuilderNoCommon();
        break;
      case REDACT:
        builder = new RedactMaskingProviderMetadataBuilder();
        break;
      case RELIGION:
        builder = new MaskingProviderMetadataBuilderBase();
        break;
      case REPLACE:
        builder = new ReplaceMaskingProviderMetadataBuilder();
        break;
      case SSN_UK:
        builder = new SSNUKMaskingProviderMetadataBuilder();
        break;
      case SSN_US:
        builder = new SSNUSMaskingProviderMetadataBuilder();
        break;
      case STATE_US:
        builder = new MaskingProviderMetadataBuilderBase();
        break;
      case SWIFT:
        builder = new SwiftMaskingProviderMetadataBuilder();
        break;
      case URL:
        builder = new URLMaskingProviderMetadataBuilder();
        break;
      case VIN:
        builder = new VINMaskingProviderMetadataBuilder();
        break;
      case ZIPCODE:
        builder = new ZipCodeMaskingProviderMetadataBuilder();
        break;
      default:
        throw new IllegalArgumentException("unknown provider type: " + provider.name());
    }
    return builder == null ? null : builder.getMaskingProviderMetadata(provider, locale);
  }
}
