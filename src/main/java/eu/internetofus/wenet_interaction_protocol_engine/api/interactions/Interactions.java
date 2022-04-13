/*
 * -----------------------------------------------------------------------------
 *
 * Copyright 2019 - 2022 UDT-IA, IIIA-CSIC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * -----------------------------------------------------------------------------
 */
package eu.internetofus.wenet_interaction_protocol_engine.api.interactions;

import eu.internetofus.common.components.interaction_protocol_engine.Interaction;
import eu.internetofus.common.components.interaction_protocol_engine.InteractionsPage;
import eu.internetofus.common.model.ErrorMessage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.enums.Explode;
import io.swagger.v3.oas.annotations.enums.ParameterStyle;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.parameters.RequestBody;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.api.service.ServiceRequest;
import io.vertx.ext.web.api.service.ServiceResponse;
import io.vertx.ext.web.api.service.WebApiServiceGen;
import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.DefaultValue;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;

/**
 * Provide services to manipulate the interactions.
 *
 * @author UDT-IA, IIIA-CSIC
 */
@Path(Interactions.PATH)
@Tag(name = "Interactions")
@WebApiServiceGen
public interface Interactions {

  /**
   * The path to the service.
   */
  String PATH = "/interactions";

  /**
   * The address of this service.
   */
  String ADDRESS = "wenet_interaction_protocol_engine.api.interactions";

  /**
   * Called when want to obtain some interactions.
   *
   * @param appId            identifier of the application where the interaction
   *                         is done.
   * @param communityId      identifier of the community where the interaction is
   *                         done.
   * @param taskTypeId       identifier of the task type where the interaction is
   *                         done.
   * @param taskId           identifier of the task where the interaction is done.
   * @param senderId         identifier of the user that has started the
   *                         interaction.
   * @param receiverId       identifier of the user that has end the interaction.
   * @param hasTransaction   this is {@code true} if the interaction requires a
   *                         transaction, {@code false} if no transaction has to
   *                         be defined or {@code null} if does not matter.
   * @param transactionLabel the label of the transaction that has started the
   *                         interaction.
   * @param transactionFrom  the minimum time stamp, inclusive, where the
   *                         interaction has to be started, or {@code null} to
   *                         start at midnight, January 1, 1970 UTC.
   * @param transactionTo    the maximum time stamp, inclusive, where the
   *                         interaction has to be started or {@code null} to be
   *                         the current time.
   * @param hasMessage       this is {@code true} if the interaction requires a
   *                         message, {@code false} if no message has to be
   *                         defined or {@code null} if does not matter.
   * @param messageLabel     the label of the message that has end the
   *                         interaction.
   * @param messageFrom      the minimum time stamp, inclusive, where the
   *                         interaction has end, or {@code null} to start at
   *                         midnight, January 1, 1970 UTC.
   * @param messageTo        the maximum time stamp, inclusive, where the
   *                         interaction has end or {@code null} to be the current
   *                         time.
   * @param order            to return the found interactions.
   * @param offset           index of the first interaction to return.
   * @param limit            number maximum of interactions to return.
   * @param context          of the request.
   * @param resultHandler    to inform of the response.
   */
  @GET
  @Produces(MediaType.APPLICATION_JSON)
  @Operation(summary = "Count the number of interactions between two users.")
  @ApiResponse(responseCode = "200", description = "The page with the interactions that match the parameters", content = @Content(schema = @Schema(implementation = InteractionsPage.class)))
  @ApiResponse(responseCode = "400", description = "Bad request parameters", content = @Content(schema = @Schema(implementation = ErrorMessage.class)))
  void getInteractionsPage(
      @QueryParam(value = "appId") @Parameter(description = "The identifier of the application where the interaction happens. You can use a Perl compatible regular expressions (PCRE) that has to match the application identifier of the interactions to return if you write between '/'. For example to get the interactions done on the applications '1' and '2' you must pass as 'appId' '/[1|2]/'", example = "1", required = false) String appId,
      @QueryParam(value = "communityId") @Parameter(description = "The identifier of the community where the interaction happens. You can use a Perl compatible regular expressions (PCRE) that has to match the community identifier of the interactions to return if you write between '/'. For example to get the interactions done on the community '1' and '2' you must pass as 'communityId' '/[1|2]/'", example = "1", required = false) String communityId,
      @QueryParam(value = "taskTypeId") @Parameter(description = "The identifier of the task type where the interaction happens. You can use a Perl compatible regular expressions (PCRE) that has to match the task type identifier of the interactions to return if you write between '/'. For example to get the interactions done on the task type '1' and '2' you must pass as 'taskTypeId' '/[1|2]/'", example = "1", required = false) String taskTypeId,
      @QueryParam(value = "taskId") @Parameter(description = "The identifier of the task where the interaction happens. You can use a Perl compatible regular expressions (PCRE) that has to match the task identifier of the interactions to return if you write between '/'. For example to get the interactions done on the task '1' and '2' you must pass as 'taskId' '/[1|2]/'", example = "1", required = false) String taskId,
      @QueryParam(value = "senderId") @Parameter(description = "The identifier of the user that has started the interaction. You can use a Perl compatible regular expressions (PCRE) that has to match the sender identifier of the interactions to return if you write between '/'. For example to get the interactions started by the users '1' and '2' you must pass as 'senderId' '/[1|2]/'", example = "1", required = false) String senderId,
      @QueryParam(value = "receiverId") @Parameter(description = "The identifier of the user that has end the interaction. You can use a Perl compatible regular expressions (PCRE) that has to match the receiver identifier of the interactions to return if you write between '/'. For example to get the interactions started by the users '1' and '2' you must pass as 'receiverId' '/[1|2]/'", example = "1", required = false) String receiverId,
      @QueryParam(value = "hasTransaction") @Parameter(description = "This is 'true' if the interaction requires a transaction.", example = "true", required = false) Boolean hasTransaction,
      @QueryParam(value = "transactionLabel") @Parameter(description = "The label of the transaction that has started the interaction. You can use a Perl compatible regular expressions (PCRE) that has to match the transaction label of the interactions to return if you write between '/'. For example to get the transaction with a label that contains the word 'eat' you must pass as 'transactionLabel' '/.*eat.*/'", example = "askMoreUsers", required = false) String transactionLabel,
      @QueryParam(value = "transactionFrom") @Parameter(description = "the minimum time stamp, inclusive, where the interaction has to be started, or {@code null} to start at midnight, January 1, 1970 UTC.", example = "1457166440", required = false) Long transactionFrom,
      @QueryParam(value = "transactionTo") @Parameter(description = "the maximum time stamp, inclusive, where the interaction has to be started or {@code null} to be the current time.", example = "1571664406", required = false) Long transactionTo,
      @QueryParam(value = "hasMessage") @Parameter(description = "This is 'true' if the interaction requires a message.", example = "true", required = false) Boolean hasMessage,
      @QueryParam(value = "messageLabel") @Parameter(description = "The label of the message that has end the interaction. You can use a Perl compatible regular expressions (PCRE) that has to match the message label of the interactions to return if you write between '/'. For example to get the message with a label that contains the word 'eat' you must pass as 'messageLabel' '/.*eat.*/'", example = "askMoreUsers", required = false) String messageLabel,
      @QueryParam(value = "messageFrom") @Parameter(description = "the minimum time stamp, inclusive, where the interaction has to be end, or {@code null} to start at midnight, January 1, 1970 UTC.", example = "1457166440", required = false) Long messageFrom,
      @QueryParam(value = "messageTo") @Parameter(description = "the maximum time stamp, inclusive, where the interaction has to be end or {@code null} to be the current time.", example = "1571664406", required = false) Long messageTo,
      @QueryParam(value = "order") @Parameter(description = "The order in witch the interactions has to be returned. For each field it has be separated by a ',' and each field can start with '+' (or without it) to order on ascending order, or with the prefix '-' to do on descendant order.", example = "transactionTs,-messageLabel", required = false, style = ParameterStyle.FORM, explode = Explode.FALSE) String order,
      @DefaultValue("0") @QueryParam(value = "offset") @Parameter(description = "The index of the first interaction to return.", example = "4", required = false) int offset,
      @DefaultValue("10") @QueryParam(value = "limit") @Parameter(description = "The number maximum of interactions to return", example = "100", required = false) int limit,
      @Parameter(hidden = true, required = false) ServiceRequest context,
      @Parameter(hidden = true, required = false) Handler<AsyncResult<ServiceResponse>> resultHandler);

  /**
   * Delete some interactions.
   *
   * @param appId            identifier of the application where the interaction
   *                         is done.
   * @param communityId      identifier of the community where the interaction is
   *                         done.
   * @param taskTypeId       identifier of the task type where the interaction is
   *                         done.
   * @param taskId           identifier of the task where the interaction is done.
   * @param senderId         identifier of the user that has started the
   *                         interaction.
   * @param receiverId       identifier of the user that has end the interaction.
   * @param hasTransaction   this is {@code true} if the interaction requires a
   *                         transaction, {@code false} if no transaction has to
   *                         be defined or {@code null} if does not matter.
   * @param transactionLabel the label of the transaction that has started the
   *                         interaction.
   * @param transactionFrom  the minimum time stamp, inclusive, where the
   *                         interaction has to be started, or {@code null} to
   *                         start at midnight, January 1, 1970 UTC.
   * @param transactionTo    the maximum time stamp, inclusive, where the
   *                         interaction has to be started or {@code null} to be
   *                         the current time.
   * @param hasMessage       this is {@code true} if the interaction requires a
   *                         message, {@code false} if no message has to be
   *                         defined or {@code null} if does not matter.
   * @param messageLabel     the label of the message that has end the
   *                         interaction.
   * @param messageFrom      the minimum time stamp, inclusive, where the
   *                         interaction has end, or {@code null} to start at
   *                         midnight, January 1, 1970 UTC.
   * @param messageTo        the maximum time stamp, inclusive, where the
   *                         interaction has end or {@code null} to be the current
   *                         time.
   * @param context          of the request.
   * @param resultHandler    to inform of the response.
   */
  @DELETE
  @Produces(MediaType.APPLICATION_JSON)
  @Operation(summary = "Delete some interactions.")
  @ApiResponse(responseCode = "204", description = "If the interaction that match the parameters has been deleted")
  @ApiResponse(responseCode = "404", description = "Can not delete because no interaction match the parameters", content = @Content(schema = @Schema(implementation = ErrorMessage.class)))
  void deleteInteractions(
      @QueryParam(value = "appId") @Parameter(description = "The identifier of the application of the interaction to delete. You can use a Perl compatible regular expressions (PCRE) that has to match the application identifier of the interactions to return if you write between '/'. For example to get the interactions done on the applications '1' and '2' you must pass as 'appId' '/[1|2]/'", example = "1", required = false) String appId,
      @QueryParam(value = "communityId") @Parameter(description = "The identifier of the community of the interaction to delete. You can use a Perl compatible regular expressions (PCRE) that has to match the community identifier of the interactions to return if you write between '/'. For example to get the interactions done on the community '1' and '2' you must pass as 'communityId' '/[1|2]/'", example = "1", required = false) String communityId,
      @QueryParam(value = "taskTypeId") @Parameter(description = "The identifier of the task type of the interaction to delete. You can use a Perl compatible regular expressions (PCRE) that has to match the task type identifier of the interactions to return if you write between '/'. For example to get the interactions done on the task type '1' and '2' you must pass as 'taskTypeId' '/[1|2]/'", example = "1", required = false) String taskTypeId,
      @QueryParam(value = "taskId") @Parameter(description = "The identifier of the task of the interaction to delete. You can use a Perl compatible regular expressions (PCRE) that has to match the task identifier of the interactions to return if you write between '/'. For example to get the interactions done on the task '1' and '2' you must pass as 'taskId' '/[1|2]/'", example = "1", required = false) String taskId,
      @QueryParam(value = "senderId") @Parameter(description = "The identifier of the user that has started the interaction to delete. You can use a Perl compatible regular expressions (PCRE) that has to match the sender identifier of the interactions to return if you write between '/'. For example to get the interactions started by the users '1' and '2' you must pass as 'senderId' '/[1|2]/'", example = "1", required = false) String senderId,
      @QueryParam(value = "receiverId") @Parameter(description = "The identifier of the user that has end the interaction to delete. You can use a Perl compatible regular expressions (PCRE) that has to match the receiver identifier of the interactions to return if you write between '/'. For example to get the interactions started by the users '1' and '2' you must pass as 'receiverId' '/[1|2]/'", example = "1", required = false) String receiverId,
      @QueryParam(value = "hasTransaction") @Parameter(description = "This is 'true' if the interaction to delete requires a transaction.", example = "true", required = false) Boolean hasTransaction,
      @QueryParam(value = "transactionLabel") @Parameter(description = "The label of the transaction that has started the interaction to delete. You can use a Perl compatible regular expressions (PCRE) that has to match the transaction label of the interactions to return if you write between '/'. For example to get the transaction with a label that contains the word 'eat' you must pass as 'transactionLabel' '/.*eat.*/'", example = "askMoreUsers", required = false) String transactionLabel,
      @QueryParam(value = "transactionFrom") @Parameter(description = "the minimum time stamp, inclusive, where the interaction to delete has to be started, or {@code null} to start at midnight, January 1, 1970 UTC.", example = "1457166440", required = false) Long transactionFrom,
      @QueryParam(value = "transactionTo") @Parameter(description = "the maximum time stamp, inclusive, where the interaction to delete has to be started or {@code null} to be the current time.", example = "1571664406", required = false) Long transactionTo,
      @QueryParam(value = "hasMessage") @Parameter(description = "This is 'true' if the interaction to delete requires a message.", example = "true", required = false) Boolean hasMessage,
      @QueryParam(value = "messageLabel") @Parameter(description = "The label of the message that has end the interaction to delete. You can use a Perl compatible regular expressions (PCRE) that has to match the message label of the interactions to return if you write between '/'. For example to get the message with a label that contains the word 'eat' you must pass as 'messageLabel' '/.*eat.*/'", example = "questionToAnswer", required = false) String messageLabel,
      @QueryParam(value = "messageFrom") @Parameter(description = "the minimum time stamp, inclusive, where the interaction to delete has to be end, or {@code null} to start at midnight, January 1, 1970 UTC.", example = "1457166440", required = false) Long messageFrom,
      @QueryParam(value = "messageTo") @Parameter(description = "the maximum time stamp, inclusive, where the interaction to delete has to be end or {@code null} to be the current time.", example = "1571664406", required = false) Long messageTo,
      @Parameter(hidden = true, required = false) ServiceRequest context,
      @Parameter(hidden = true, required = false) Handler<AsyncResult<ServiceResponse>> resultHandler);

  /**
   * Add a new interaction.
   *
   * @param body          interaction to store.
   * @param context       of the request.
   * @param resultHandler to inform of the response.
   */
  @POST
  @Produces(MediaType.APPLICATION_JSON)
  @Consumes(MediaType.APPLICATION_JSON)
  @Operation(summary = "Add a new interaction between users.")
  @RequestBody(description = "The interaction to add", required = true, content = @Content(schema = @Schema(implementation = Interaction.class)))
  @ApiResponse(responseCode = "201", description = "The stored interaction", content = @Content(schema = @Schema(implementation = Interaction.class)))
  @ApiResponse(responseCode = "400", description = "Bad interaction to add", content = @Content(schema = @Schema(implementation = ErrorMessage.class)))
  void addInteraction(@Parameter(hidden = true, required = false) JsonObject body,
      @Parameter(hidden = true, required = false) ServiceRequest context,
      @Parameter(hidden = true, required = false) Handler<AsyncResult<ServiceResponse>> resultHandler);

}
