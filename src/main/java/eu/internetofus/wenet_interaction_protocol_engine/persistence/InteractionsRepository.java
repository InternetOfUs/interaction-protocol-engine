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

package eu.internetofus.wenet_interaction_protocol_engine.persistence;

import eu.internetofus.common.components.interaction_protocol_engine.Interaction;
import eu.internetofus.common.components.interaction_protocol_engine.InteractionsPage;
import eu.internetofus.common.model.Model;
import eu.internetofus.common.model.ValidationErrorException;
import eu.internetofus.common.vertx.ModelsPageContext;
import eu.internetofus.common.vertx.QueryBuilder;
import eu.internetofus.common.vertx.Repository;
import io.vertx.codegen.annotations.GenIgnore;
import io.vertx.codegen.annotations.ProxyGen;
import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Promise;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.mongo.MongoClient;
import io.vertx.serviceproxy.ServiceBinder;
import java.util.List;

/**
 * The service to manage the {@link Interaction} on the database.
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ProxyGen
public interface InteractionsRepository {

  /**
   * The address of this service.
   */
  String ADDRESS = "wenet_interaction_protocol_engine.persistence.interactions";

  /**
   * Register this service.
   *
   * @param vertx   that contains the event bus to use.
   * @param pool    to create the database connections.
   * @param version of the schemas.
   *
   * @return the future that inform when the repository will be registered or not.
   */
  static Future<Void> register(final Vertx vertx, final MongoClient pool, final String version) {

    final var repository = new InteractionsRepositoryImpl(vertx, pool, version);
    new ServiceBinder(vertx).setAddress(InteractionsRepository.ADDRESS).register(InteractionsRepository.class,
        repository);
    return repository.migrateDocumentsToCurrentVersions();

  }

  /**
   * Create a proxy of the {@link InteractionsRepository}.
   *
   * @param vertx where the service has to be used.
   *
   * @return the interaction.
   */
  static InteractionsRepository createProxy(final Vertx vertx) {

    return new InteractionsRepositoryVertxEBProxy(vertx, InteractionsRepository.ADDRESS);
  }

  /**
   * Create the query to askType about some taskTypes.
   *
   * @param appId            the pattern to match the {@link Interaction#appId}.
   * @param communityId      the pattern to match the
   *                         {@link Interaction#communityId}.
   * @param taskTypeId       the pattern to match the
   *                         {@link Interaction#taskTypeId}.
   * @param taskId           the pattern to match the {@link Interaction#taskId}.
   * @param senderId         the pattern to match the
   *                         {@link Interaction#senderId}.
   * @param receiverId       the pattern to match the
   *                         {@link Interaction#receiverId}.
   * @param hasTransaction   this is {@code true} if the interaction requires a
   *                         transaction.
   * @param transactionLabel the pattern to match the {@link Interaction#appId}.
   * @param transactionFrom  the minimum time stamp, inclusive, where the
   *                         interaction has to be started, or {@code null} to
   *                         start at midnight, January 1, 1970 UTC.
   * @param transactionTo    the maximum time stamp, inclusive, where the
   *                         interaction has to be started or {@code null} to be
   *                         the current time.
   * @param hasMessage       this is {@code true} if the interaction requires a
   *                         message.
   * @param messageLabel     the pattern to match the
   *                         {@link Interaction#messageLabel}.
   * @param messageFrom      the minimum time stamp, inclusive, where the
   *                         interaction has end, or {@code null} to start at
   *                         midnight, January 1, 1970 UTC.
   * @param messageTo        the maximum time stamp, inclusive, where the
   *                         interaction has end or {@code null} to be the current
   *                         time.
   *
   * @return the query that you have to use to obtains some taskTypes.
   */
  static JsonObject createInteractionsPageQuery(final String appId, final String communityId, final String taskTypeId,
      final String taskId, final String senderId, final String receiverId, final Boolean hasTransaction,
      final String transactionLabel, final Long transactionFrom, final Long transactionTo, final Boolean hasMessage,
      final String messageLabel, final Long messageFrom, final Long messageTo) {

    return new QueryBuilder().withEqOrRegex("appId", appId).withEqOrRegex("communityId", communityId)
        .withEqOrRegex("taskTypeId", taskTypeId).withEqOrRegex("taskId", taskId).withEqOrRegex("senderId", senderId)
        .withEqOrRegex("receiverId", receiverId).withEqOrRegex("transactionLabel", transactionLabel)
        .withEqOrRegex("messageLabel", messageLabel).withRange("transactionTs", transactionFrom, transactionTo)
        .withRange("messageTs", messageFrom, messageTo).withExist("transactionLabel", hasTransaction)
        .withExist("messageLabel", hasMessage).build();

  }

  /**
   * Create the sort query of the task type.
   *
   * @param order to sort the task types.
   *
   * @return the sort query of the task types page.
   *
   * @throws ValidationErrorException If the values are not right.
   */
  static Future<JsonObject> createInteractionsPageSort(final List<String> order) {

    final Promise<JsonObject> promise = Promise.promise();
    try {

      final var sort = Repository.queryParamToSort(order, "bad_order", (value) -> {

        switch (value) {
        case "appId":
        case "communityId":
        case "taskTypeId":
        case "taskId":
        case "senderId":
        case "receiverId":
        case "transactionLabel":
        case "transactionTs":
        case "messageLabel":
        case "messageTs":
          return value;
        default:
          return null;
        }

      });
      promise.complete(sort);

    } catch (final ValidationErrorException error) {

      promise.fail(error);
    }

    return promise.future();

  }

  /**
   * Store an interaction.
   *
   * @param interaction to store.
   *
   * @return the future with the stored interaction.
   *
   * @see #storeInteraction(JsonObject, Handler)
   */
  @GenIgnore
  default Future<Interaction> store(final Interaction interaction) {

    final var document = interaction.toJsonObject();
    final Promise<JsonObject> promise = Promise.promise();
    this.storeInteraction(document, promise);
    return Model.fromFutureJsonObject(promise.future(), Interaction.class);

  }

  /**
   * Store an interaction.
   *
   * @param interaction  to store.
   * @param storeHandler handler to manage the search.
   */
  void storeInteraction(JsonObject interaction, Handler<AsyncResult<JsonObject>> storeHandler);

  /**
   * Retrieve the interactions defined on the context.
   *
   * @param context that describe witch page want to obtain.
   *
   * @return the future page.
   */
  @GenIgnore
  default Future<InteractionsPage> retrieveInteractionsPage(final ModelsPageContext context) {

    return this.retrieveInteractionsPage(context.query, context.sort, context.offset, context.limit);

  }

  /**
   * Retrieve the interactions defined on the context.
   *
   * @param query  to obtain the required interactions.
   * @param sort   describe how has to be ordered the obtained interactions.
   * @param offset the index of the first community to return.
   * @param limit  the number maximum of interactions to return.
   *
   * @return the future page.
   */
  @GenIgnore
  default Future<InteractionsPage> retrieveInteractionsPage(final JsonObject query, final JsonObject sort,
      final int offset, final int limit) {

    final Promise<JsonObject> promise = Promise.promise();
    this.retrieveInteractionsPage(query, sort, offset, limit, promise);
    return Model.fromFutureJsonObject(promise.future(), InteractionsPage.class);

  }

  /**
   * Retrieve a page with some interactions.
   *
   * @param query   to obtain the required interactions.
   * @param sort    describe how has to be ordered the obtained interactions.
   * @param offset  the index of the first community to return.
   * @param limit   the number maximum of interactions to return.
   * @param handler to inform of the found interactions.
   */
  void retrieveInteractionsPage(JsonObject query, JsonObject sort, int offset, int limit,
      Handler<AsyncResult<JsonObject>> handler);

  /**
   * Remove all the interactions that satisfy the query.
   *
   * @param query that match the interaction to remove.
   *
   * @return the future to inform if the interaction are removed or not.
   */
  @GenIgnore
  default Future<Void> deleteInteractions(final JsonObject query) {

    final Promise<Void> promise = Promise.promise();
    this.deleteInteractions(query, promise);
    return promise.future();

  }

  /**
   * Remove all the interactions that satisfy the query.
   *
   * @param query   that match the interaction to remove.
   * @param handler to inform if the interaction are removed or not.
   */
  void deleteInteractions(JsonObject query, Handler<AsyncResult<Void>> handler);

}
