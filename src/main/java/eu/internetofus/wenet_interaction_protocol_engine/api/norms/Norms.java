/*
 * -----------------------------------------------------------------------------
 *
 * Copyright (c) 2019 - 2022 UDT-IA, IIIA-CSIC
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * -----------------------------------------------------------------------------
 */

package eu.internetofus.wenet_interaction_protocol_engine.api.norms;

import java.util.List;

import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.DefaultValue;
import javax.ws.rs.GET;
import javax.ws.rs.PATCH;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;

import eu.internetofus.common.components.ErrorMessage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.enums.Explode;
import io.swagger.v3.oas.annotations.enums.ParameterIn;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.parameters.RequestBody;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.api.OperationRequest;
import io.vertx.ext.web.api.OperationResponse;
import io.vertx.ext.web.api.generator.WebApiServiceGen;

/**
 * The definition of the web services for manage the norms.
 *
 * @author UDT-IA, IIIA-CSIC
 */
@Path(Norms.PATH)
@Tag(name = "Norms")
@WebApiServiceGen
public interface Norms {

  /**
   * The path to the version resource.
   */
  String PATH = "/norms";

  /**
   * The address of this service.
   */
  String ADDRESS = "wenet_interaction_protocol_engine.api.norms";

  /**
   * The sub path to retrieve a published norm.
   */
  String PUBLISHED_NORM_ID_PATH = "/{publishedNormId}";

  /**
   * Called when want to publish a norm.
   *
   * @param body          the new norm to publish.
   * @param context       of the request.
   * @param resultHandler to inform of the response.
   */
  @POST
  @Consumes(MediaType.APPLICATION_JSON)
  @Produces(MediaType.APPLICATION_JSON)
  @Operation(summary = "Publish a norm", description = "Publish a norm to be accessible by the other users on WeNet")
  @RequestBody(description = "The norm to publish", required = true, content = @Content(schema = @Schema(implementation = PublishedNorm.class)))
  @ApiResponse(responseCode = "201", description = "The published norm", content = @Content(schema = @Schema(implementation = PublishedNorm.class)))
  @ApiResponse(responseCode = "400", description = "Bad norm to publish", content = @Content(schema = @Schema(implementation = ErrorMessage.class)))
  void publishNorm(@Parameter(hidden = true, required = false) JsonObject body, @Parameter(hidden = true, required = false) OperationRequest context,
      @Parameter(hidden = true, required = false) Handler<AsyncResult<OperationResponse>> resultHandler);

  /**
   * Called when want to get some published norms.
   *
   * @param name          of the norms to return.
   * @param description   of the norms to return.
   * @param keywords      of the norms to return.
   * @param publisherId   identifier of the user that has published the norm.
   * @param publishFrom   minimal deadline time stamp of the tasks to return.
   * @param publishTo     maximal deadline time stamp of the tasks to return.
   * @param order         of the norms to return.
   * @param offset        index of the first norm to return.
   * @param limit         number maximum of norm to return.
   * @param context       of the request.
   * @param resultHandler to inform of the response.
   */
  @GET
  @Produces(MediaType.APPLICATION_JSON)
  @Operation(summary = "Return some published norms", description = "Allow to search for a published norms that match some patterns")
  @Parameter(in = ParameterIn.QUERY, name = "keyword", description = "The keyword of the published norms to return or a Perl compatible regular expressions (PCRE) that has to match the keyword of the published norms to return.", required = false, array = @ArraySchema(schema = @Schema(type = "string", example = "(?i)key(?-i)word")))
  @ApiResponse(responseCode = "200", description = "The published norms that match the patterns", content = @Content(schema = @Schema(implementation = PublishedNormsPage.class)))
  @ApiResponse(responseCode = "400", description = "Bad request. For example if a pattern is not right", content = @Content(schema = @Schema(implementation = ErrorMessage.class)))
  void retrievePublishedNormsPage(
      @QueryParam(value = "name") @Parameter(description = "A name to be equals on the published norms to return. You can use a Perl compatible regular expressions (PCRE) that has to match the name of the published norms to return if you write between '/'. For example to get the published norms with a name with the word 'eat' you must pass as 'name' '/.*eat.*/'", example = "/.*eat.*/", required = false) String name,
      @QueryParam(value = "description") @Parameter(description = "A description to be equals on the published norms to return. You can use a Perl compatible regular expressions (PCRE) that has to match the description of the published norms to return if you write between '/'. For example to get the published norms with a description with the word 'eat' you must pass as 'description' '/.*eat.*/'", example = "/.*eat.*/", required = false) String description,
      @QueryParam(value = "keywords") @Parameter(description = "A set of keywords to be defined on the published norms to be returned. For each keyword is separated by a ',' and each field keyword can be between '/' to use a Perl compatible regular expressions (PCRE) instead the exact value.", example = "key1,/.*eat.*/,key3", required = false, explode = Explode.FALSE) List<String> keywords,
      @QueryParam(value = "publisherId") @Parameter(description = "An user identifier to be equals on the publisher of the norm to return. You can use a Perl compatible regular expressions (PCRE) that has to match the publisher identifier of the norms to return if you write between '/'. For example to get the norms published by '1' and '2' you must pass as 'publisherId' '/^[1|2]$/'.", example = "1e346fd440", required = false) String publisherId,
      @QueryParam(value = "publishFrom") @Parameter(description = "The difference, measured in seconds, between the minimum publish time stamp of the task and midnight, January 1, 1970 UTC.", example = "1457166440", required = false) Long publishFrom,
      @QueryParam(value = "publishTo") @Parameter(description = "The difference, measured in seconds, between the maximum publish time stamp of the task and midnight, January 1, 1970 UTC.", example = "1571664406", required = false) Long publishTo,
      @QueryParam(value = "order") @Parameter(description = "The order in witch the norms has to be returned. For each field it has be separated by a ',' and each field can start with '+' (or without it) to order on ascending order, or with the prefix '-' to do on descendant order.", example = "name,-description,+publisherId", required = false, explode = Explode.FALSE) List<String> order,
      @DefaultValue("0") @QueryParam(value = "offset") @Parameter(description = "The index of the first norm to return.", example = "4", required = false) int offset,
      @DefaultValue("10") @QueryParam(value = "limit") @Parameter(description = "The number maximum of norms to return", example = "100", required = false) int limit, @Parameter(hidden = true, required = false) OperationRequest context,
      @Parameter(hidden = true, required = false) Handler<AsyncResult<OperationResponse>> resultHandler);

  /**
   * Called when want to get a published norm.
   *
   * @param publishedNormId identifier of the published norm to get.
   * @param context         of the request.
   * @param resultHandler   to inform of the response.
   */
  @GET
  @Path(PUBLISHED_NORM_ID_PATH)
  @Produces(MediaType.APPLICATION_JSON)
  @Operation(summary = "Return a published norm associated to the identifier", description = "Allow to get a published norm associated to an identifier")
  @ApiResponse(responseCode = "200", description = "The published norm associated to the identifier", content = @Content(schema = @Schema(implementation = PublishedNorm.class)))
  @ApiResponse(responseCode = "404", description = "Not found published norm", content = @Content(schema = @Schema(implementation = ErrorMessage.class)))
  void retrievePublishedNorm(@PathParam("publishedNormId") @Parameter(description = "The identifier of the published norm to get", example = "15837028-645a-4a55-9aaf-ceb846439eba") String publishedNormId,
      @Parameter(hidden = true, required = false) OperationRequest context, @Parameter(hidden = true, required = false) Handler<AsyncResult<OperationResponse>> resultHandler);

  /**
   * Called when want to modify completly a published norm.
   *
   * @param publishedNormId identifier of the published norm to modify.
   * @param body            the new published norm attributes.
   * @param context         of the request.
   * @param resultHandler   to inform of the response.
   */
  @PUT
  @Path(PUBLISHED_NORM_ID_PATH)
  @Consumes(MediaType.APPLICATION_JSON)
  @Produces(MediaType.APPLICATION_JSON)
  @Operation(summary = "Modify all the fields of a published norm", description = "Change all the attributes of a published norm")
  @RequestBody(description = "The new values for the published norm", required = true, content = @Content(schema = @Schema(implementation = PublishedNorm.class)))
  @ApiResponse(responseCode = "200", description = "The updated published norm", content = @Content(schema = @Schema(implementation = PublishedNorm.class)))
  @ApiResponse(responseCode = "400", description = "Bad published norm", content = @Content(schema = @Schema(implementation = ErrorMessage.class)))
  @ApiResponse(responseCode = "404", description = "Not found published norm", content = @Content(schema = @Schema(implementation = ErrorMessage.class)))
  void updatePublishedNorm(@PathParam("publishedNormId") @Parameter(description = "The identifier of the published norm to update", example = "15837028-645a-4a55-9aaf-ceb846439eba") String publishedNormId,
      @Parameter(hidden = true, required = false) JsonObject body, @Parameter(hidden = true, required = false) OperationRequest context, @Parameter(hidden = true, required = false) Handler<AsyncResult<OperationResponse>> resultHandler);

  /**
   * Called when want to modify partially a published norm.
   *
   * @param publishedNormId identifier of the published norm to merge.
   * @param body            the new attributes to merge.
   * @param context         of the request.
   * @param resultHandler   to inform of the response.
   */
  @PATCH
  @Path(PUBLISHED_NORM_ID_PATH)
  @Consumes(MediaType.APPLICATION_JSON)
  @Produces(MediaType.APPLICATION_JSON)
  @Operation(summary = "Modify partially a published norm", description = "Change some of the attributes of a published norm")
  @RequestBody(description = "The new values for the published norm", required = true, content = @Content(schema = @Schema(implementation = PublishedNorm.class)))
  @ApiResponse(responseCode = "200", description = "The merged published norm", content = @Content(schema = @Schema(implementation = PublishedNorm.class)))
  @ApiResponse(responseCode = "400", description = "Bad published norm to merge", content = @Content(schema = @Schema(implementation = ErrorMessage.class)))
  @ApiResponse(responseCode = "404", description = "Not found published norm to merge", content = @Content(schema = @Schema(implementation = ErrorMessage.class)))
  void mergePublishedNorm(@PathParam("publishedNormId") @Parameter(description = "The identifier of the published norm to merge", example = "15837028-645a-4a55-9aaf-ceb846439eba") String publishedNormId,
      @Parameter(hidden = true, required = false) JsonObject body, @Parameter(hidden = true, required = false) OperationRequest context, @Parameter(hidden = true, required = false) Handler<AsyncResult<OperationResponse>> resultHandler);

  /**
   * Called when want to delete a published norm.
   *
   * @param publishedNormId identifier of the published norm to delete.
   * @param context         of the request.
   * @param resultHandler   to inform of the response.
   */
  @DELETE
  @Path(PUBLISHED_NORM_ID_PATH)
  @Produces(MediaType.APPLICATION_JSON)
  @Operation(summary = "Delete the published norm associated to the identifier", description = "Allow to delete a published norm associated to an identifier")
  @ApiResponse(responseCode = "204", description = "The published norm was deleted successfully")
  @ApiResponse(responseCode = "404", description = "Not found published norm", content = @Content(schema = @Schema(implementation = ErrorMessage.class)))
  void deletePublishedNorm(@PathParam("publishedNormId") @Parameter(description = "The identifier of the published norm to delete") String publishedNormId, @Parameter(hidden = true, required = false) OperationRequest context,
      @Parameter(hidden = true, required = false) Handler<AsyncResult<OperationResponse>> resultHandler);

}
