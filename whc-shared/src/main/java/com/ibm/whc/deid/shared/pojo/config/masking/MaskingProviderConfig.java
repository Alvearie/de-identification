/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonSubTypes.Type;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderType;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderTypes;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;
import java.io.Serializable;

/*
 * Abstract superclass for all masking provider configuration classes.
 */
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = "type",
    visible = true)
@JsonSubTypes({
    @Type(value = AddressMaskingProviderConfig.class,
        name = MaskingProviderType.Constants.ADDRESS_VALUE),
    @Type(value = ATCMaskingProviderConfig.class, name = MaskingProviderType.Constants.ATC_VALUE),
    @Type(value = BinningMaskingProviderConfig.class,
        name = MaskingProviderType.Constants.BINNING_VALUE),
    @Type(value = CityMaskingProviderConfig.class, name = MaskingProviderType.Constants.CITY_VALUE),
    @Type(value = ConditionalMaskingProviderConfig.class,
        name = MaskingProviderType.Constants.CONDITIONAL_VALUE),
    @Type(value = ContinentMaskingProviderConfig.class,
        name = MaskingProviderType.Constants.CONTINENT_VALUE),
    @Type(value = CountyMaskingProviderConfig.class,
        name = MaskingProviderType.Constants.COUNTY_VALUE),
    @Type(value = CountryMaskingProviderConfig.class,
        name = MaskingProviderType.Constants.COUNTRY_VALUE),
    @Type(value = CreditCardMaskingProviderConfig.class,
        name = MaskingProviderType.Constants.CREDIT_CARD_VALUE),
    @Type(value = DateDependencyMaskingProviderConfig.class,
        name = MaskingProviderType.Constants.DATEDEPENDENCY_VALUE),
    @Type(value = DateTimeMaskingProviderConfig.class,
        name = MaskingProviderType.Constants.DATETIME_VALUE),
    @Type(value = DateTimeConsistentShiftMaskingProviderConfig.class,
        name = MaskingProviderType.Constants.DATETIME_CONSISTENT_SHIFT_VALUE),
    @Type(value = EmailMaskingProviderConfig.class,
        name = MaskingProviderType.Constants.EMAIL_VALUE),
    @Type(value = FPEMaskingProviderConfig.class, name = MaskingProviderType.Constants.FPE_VALUE),
    @Type(value = GenderMaskingProviderConfig.class,
        name = MaskingProviderType.Constants.GENDER_VALUE),
    @Type(value = GeneralizeMaskingProviderConfig.class,
        name = MaskingProviderType.Constants.GENERALIZE_VALUE),
    @Type(value = GUIDMaskingProviderConfig.class, name = MaskingProviderType.Constants.GUID_VALUE),
    @Type(value = HashMaskingProviderConfig.class, name = MaskingProviderType.Constants.HASH_VALUE),
    @Type(value = HospitalMaskingProviderConfig.class,
        name = MaskingProviderType.Constants.HOSPITAL_VALUE),
    @Type(value = IPAddressMaskingProviderConfig.class,
        name = MaskingProviderType.Constants.IP_ADDRESS_VALUE),
    @Type(value = IBANMaskingProviderConfig.class, name = MaskingProviderType.Constants.IBAN_VALUE),
    @Type(value = ICDv9MaskingProviderConfig.class,
        name = MaskingProviderType.Constants.ICDV9_VALUE),
    @Type(value = ICDv10MaskingProviderConfig.class,
        name = MaskingProviderType.Constants.ICDV10_VALUE),
    @Type(value = IMEIMaskingProviderConfig.class, name = MaskingProviderType.Constants.IMEI_VALUE),
    @Type(value = LatitudeLongitudeMaskingProviderConfig.class,
        name = MaskingProviderType.Constants.LATITUDE_LONGITUDE_VALUE),
    @Type(value = MACAddressMaskingProviderConfig.class,
        name = MaskingProviderType.Constants.MAC_ADDRESS_VALUE),
    @Type(value = MaintainMaskingProviderConfig.class,
        name = MaskingProviderType.Constants.MAINTAIN_VALUE),
    @Type(value = MaritalStatusMaskingProviderConfig.class,
        name = MaskingProviderType.Constants.MARITAL_VALUE),
    @Type(value = NameMaskingProviderConfig.class, name = MaskingProviderType.Constants.NAME_VALUE),
    @Type(value = NumberVarianceMaskingProviderConfig.class,
        name = MaskingProviderType.Constants.NUMBERVARIANCE_VALUE),
    @Type(value = NullMaskingProviderConfig.class, name = MaskingProviderType.Constants.NULL_VALUE),
    @Type(value = OccupationMaskingProviderConfig.class,
        name = MaskingProviderType.Constants.OCCUPATION_VALUE),
    @Type(value = PhoneMaskingProviderConfig.class,
        name = MaskingProviderType.Constants.PHONE_VALUE),
    @Type(value = PseudonymMaskingProviderConfig.class,
        name = MaskingProviderType.Constants.PSEUDONYM_VALUE),
    @Type(value = RaceEthnicityMaskingProviderConfig.class,
        name = MaskingProviderType.Constants.RACE_VALUE),
    @Type(value = RandomMaskingProviderConfig.class,
        name = MaskingProviderType.Constants.RANDOM_VALUE),
    @Type(value = RedactMaskingProviderConfig.class,
        name = MaskingProviderType.Constants.REDACT_VALUE),
    @Type(value = ReligionMaskingProviderConfig.class,
        name = MaskingProviderType.Constants.RELIGION_VALUE),
    @Type(value = ReplaceMaskingProviderConfig.class,
        name = MaskingProviderType.Constants.REPLACE_VALUE),
    @Type(value = SSNUKMaskingProviderConfig.class,
        name = MaskingProviderType.Constants.SSN_UK_VALUE),
    @Type(value = SSNUSMaskingProviderConfig.class,
        name = MaskingProviderType.Constants.SSN_US_VALUE),
    @Type(value = StatesUSMaskingProviderConfig.class,
        name = MaskingProviderType.Constants.STATE_US_VALUE),
    @Type(value = SWIFTMaskingProviderConfig.class,
        name = MaskingProviderType.Constants.SWIFT_VALUE),
    @Type(value = URLMaskingProviderConfig.class, name = MaskingProviderType.Constants.URL_VALUE),
    @Type(value = VINMaskingProviderConfig.class, name = MaskingProviderType.Constants.VIN_VALUE),
    @Type(value = ZIPCodeMaskingProviderConfig.class,
        name = MaskingProviderType.Constants.ZIPCODE_VALUE)})
public abstract class MaskingProviderConfig implements Serializable {

  private static final long serialVersionUID = 4415528180767346093L;

  /**
   * Use null as the masked value.
   * 
   * Use UnexpectedMaskingInputHandler enumeration instead.
   */
  public static final int UNSPECIFIED_VALUE_HANDLING_NULL = 1;

  /**
   * Use a random value generated by the privacy provider as the masked value.
   * 
   * Use UnexpectedMaskingInputHandler enumeration instead.
   */
  public static final int UNSPECIFIED_VALUE_HANDLING_RANDOM = 2;

  /**
   * Use the configured value/message as the masked value.
   * 
   * Use UnexpectedMaskingInputHandler enumeration instead.
   */
  public static final int UNSPECIFIED_VALUE_HANDLING_MESSAGE = 3;

  /**
   * Default value of masking config unspecified return message
   */
  public static final String UNSPECIFIED_VALUE_RETURN_MESSAGE_OTHER = "OTHER";

  /**
   * Default value of masking config unexpected input return message
   */
  public static final String UNEXPECTED_INPUT_RETURN_MESSAGE_OTHER = "OTHER";

  protected MaskingProviderTypes type;

  protected int unspecifiedValueHandling;
  protected String unspecifiedValueReturnMessage = UNSPECIFIED_VALUE_RETURN_MESSAGE_OTHER;

  protected UnexpectedMaskingInputHandler unexpectedInputHandling = null;
  protected String unexpectedInputReturnMessage = UNEXPECTED_INPUT_RETURN_MESSAGE_OTHER;

  // Jackson already set the type value when property and visible are set in
  // @JsonTypeInfo
  @JsonIgnore
  public MaskingProviderTypes getType() {
    return type;
  }

  public void setType(MaskingProviderTypes type) {
    this.type = type;
  }

  public int getUnspecifiedValueHandling() {
    return unspecifiedValueHandling;
  }

  public void setUnspecifiedValueHandling(int unspecifiedValueHandling) {
    this.unspecifiedValueHandling = unspecifiedValueHandling;
  }

  public String getUnspecifiedValueReturnMessage() {
    return unspecifiedValueReturnMessage;
  }

  public void setUnspecifiedValueReturnMessage(String unspecifiedValueReturnMessage) {
    this.unspecifiedValueReturnMessage = unspecifiedValueReturnMessage;
  }

  public UnexpectedMaskingInputHandler getUnexpectedInputHandling() {
    return unexpectedInputHandling;
  }

  public void setUnexpectedInputHandling(UnexpectedMaskingInputHandler unexpectedInputHandling) {
    this.unexpectedInputHandling = unexpectedInputHandling;
  }

  public String getUnexpectedInputReturnMessage() {
    return unexpectedInputReturnMessage;
  }

  public void setUnexpectedInputReturnMessage(String unexpectedInputReturnMessage) {
    this.unexpectedInputReturnMessage = unexpectedInputReturnMessage;
  }

  /**
   * Determines if this masking provider configuration is valid. A configuration might be invalid if
   * it has been given conflicting information or is missing required information.
   * 
   * <p>
   * Subclasses requiring additional validation should override this method, but should still ensure
   * the validation done here is performed if applicable.
   * 
   * @param maskingConfig the complete masking configuration - for reference only, the
   *        implementation is only responsible for validating the configuration that has already
   *        been set on itself
   * 
   * @throws InvalidMaskingConfigurationException if this masking provider configuration is
   *         currently invalid
   */
  public void validate(DeidMaskingConfig maskingConfig)
      throws InvalidMaskingConfigurationException {
    // check new handling property first - if not provided, validate the old one
    if (unexpectedInputHandling == null) {
      if (unspecifiedValueHandling < 0 || unspecifiedValueHandling > 3) {
        throw new InvalidMaskingConfigurationException("`unspecifiedValueHandling` must be [0..3]");
      }
    }
  }

  /**
   * Get the default masking config for various providers that have default config. Some masking
   * providers do not have default configuration such as FHIRMaskingProvider
   *
   * @param type
   * @return
   */
  public static MaskingProviderConfig getDefaultMaskingProviderConfig(MaskingProviderType type) {
    switch (type) {
      case ADDRESS:
        return new AddressMaskingProviderConfig();
      case ATC:
        return new ATCMaskingProviderConfig();
      case BINNING:
        return new BinningMaskingProviderConfig();
      case CITY:
        return new CityMaskingProviderConfig();
      case CONDITIONAL:
        return new ConditionalMaskingProviderConfig();
      case CONTINENT:
        return new ContinentMaskingProviderConfig();
      case COUNTY:
        return new CountyMaskingProviderConfig();
      case COUNTRY:
        return new CountryMaskingProviderConfig();
      case CREDIT_CARD:
        return new CreditCardMaskingProviderConfig();
      case DATEDEPENDENCY:
        return new DateDependencyMaskingProviderConfig();
      case DATETIME:
        return new DateTimeMaskingProviderConfig();
      case DATETIME_CONSISTENT_SHIFT:
        return new DateTimeConsistentShiftMaskingProviderConfig();
      case EMAIL:
        return new EmailMaskingProviderConfig();
      case GENDER:
        return new GenderMaskingProviderConfig();
      case GENERALIZE:
        return new GeneralizeMaskingProviderConfig();
      case GUID:
        return new GUIDMaskingProviderConfig();
      case HASH:
        return new HashMaskingProviderConfig();
      case HOSPITAL:
        return new HospitalMaskingProviderConfig();
      case IBAN:
        return new IBANMaskingProviderConfig();
      case ICDV9:
        return new ICDv9MaskingProviderConfig();
      case ICDV10:
        return new ICDv10MaskingProviderConfig();
      case IMEI:
        return new IMEIMaskingProviderConfig();
      case IP_ADDRESS:
        return new IPAddressMaskingProviderConfig();
      case LATITUDE_LONGITUDE:
        return new LatitudeLongitudeMaskingProviderConfig();
      case MAC_ADDRESS:
        return new MACAddressMaskingProviderConfig();
      case MAINTAIN:
        return new MaintainMaskingProviderConfig();
      case MARITAL:
        return new MaritalStatusMaskingProviderConfig();
      case NAME:
        return new NameMaskingProviderConfig();
      case NULL:
        return new NullMaskingProviderConfig();
      case NUMBERVARIANCE:
        return new NumberVarianceMaskingProviderConfig();
      case OCCUPATION:
        return new OccupationMaskingProviderConfig();
      case PHONE:
        return new PhoneMaskingProviderConfig();
      case PSEUDONYM:
        return new PseudonymMaskingProviderConfig();
      case RACE:
        return new RaceEthnicityMaskingProviderConfig();
      case RANDOM:
        return new RandomMaskingProviderConfig();
      case REDACT:
        return new RedactMaskingProviderConfig();
      case RELIGION:
        return new ReligionMaskingProviderConfig();
      case REPLACE:
        return new ReplaceMaskingProviderConfig();
      case SSN_UK:
        return new SSNUKMaskingProviderConfig();
      case SSN_US:
        return new SSNUSMaskingProviderConfig();
      case STATE_US:
        return new StatesUSMaskingProviderConfig();
      case SWIFT:
        return new SWIFTMaskingProviderConfig();
      case URL:
        return new URLMaskingProviderConfig();
      case VIN:
        return new VINMaskingProviderConfig();
      case ZIPCODE:
        return new ZIPCodeMaskingProviderConfig();

      // FHIR/GEN are complex masking providers which do not have
      // masking provider config
      case FHIR:
      case GEN:
      default:
        throw new IllegalArgumentException("Unsupported provider type" + type);
    }
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((type == null) ? 0 : type.hashCode());
    result = prime * result
        + ((unexpectedInputHandling == null) ? 0 : unexpectedInputHandling.hashCode());
    result = prime * result
        + ((unexpectedInputReturnMessage == null) ? 0 : unexpectedInputReturnMessage.hashCode());
    result = prime * result + unspecifiedValueHandling;
    result = prime * result
        + ((unspecifiedValueReturnMessage == null) ? 0 : unspecifiedValueReturnMessage.hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj)
      return true;
    if (obj == null)
      return false;
    if (getClass() != obj.getClass())
      return false;
    MaskingProviderConfig other = (MaskingProviderConfig) obj;
    if (type == null) {
      if (other.type != null)
        return false;
    } else if (!type.equals(other.type))
      return false;
    if (unexpectedInputHandling != other.unexpectedInputHandling)
      return false;
    if (unexpectedInputReturnMessage == null) {
      if (other.unexpectedInputReturnMessage != null)
        return false;
    } else if (!unexpectedInputReturnMessage.equals(other.unexpectedInputReturnMessage))
      return false;
    if (unspecifiedValueHandling != other.unspecifiedValueHandling)
      return false;
    if (unspecifiedValueReturnMessage == null) {
      if (other.unspecifiedValueReturnMessage != null)
        return false;
    } else if (!unspecifiedValueReturnMessage.equals(other.unspecifiedValueReturnMessage))
      return false;
    return true;
  }
}
