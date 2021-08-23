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

import eu.internetofus.common.components.interaction_protocol_engine.State;
import eu.internetofus.common.model.Model;
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

/**
 * The service to manage the {@link State} on the database.
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ProxyGen
public interface StatesRepository {

  /**
   * The address of this service.
   */
  String ADDRESS = "wenet_interaction_protocol_engine.persistence.states";

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

    final var repository = new StatesRepositoryImpl(vertx, pool, version);
    new ServiceBinder(vertx).setAddress(StatesRepository.ADDRESS).register(StatesRepository.class, repository);
    return repository.migrateDocumentsToCurrentVersions();

  }

  /**
   * Create a proxy of the {@link StatesRepository}.
   *
   * @param vertx where the service has to be used.
   *
   * @return the state.
   */
  static StatesRepository createProxy(final Vertx vertx) {

    return new StatesRepositoryVertxEBProxy(vertx, StatesRepository.ADDRESS);
  }

  /**
   * Search for the state with the specified attributes.
   *
   * @param communityId identifier of the community in the state.
   * @param taskId      identifier of the task in the state.
   * @param userId      identifier of the user in the state.
   * @return the future search model.
   */
  @GenIgnore
  default Future<State> searchState(final String communityId, final String taskId, final String userId) {

    final Promise<JsonObject> promise = Promise.promise();
    this.searchState(communityId, taskId, userId, promise);
    return Model.fromFutureJsonObject(promise.future(), State.class);

  }

  /**
   * Search for the state with the specified identifier.
   *
   * @param communityId   identifier of the community in the state.
   * @param taskId        identifier of the task in the state.
   * @param userId        identifier of the user in the state.
   * @param searchHandler handler to manage the search.
   */
  void searchState(final String communityId, String taskId, String userId,
      Handler<AsyncResult<JsonObject>> searchHandler);

  /**
   * Store a state.
   *
   * @param state to store.
   *
   * @return the future stored state.
   */
  @GenIgnore
  default Future<State> storeState(final State state) {

    final Promise<JsonObject> promise = Promise.promise();
    final var object = state.toJsonObject();
    if (object == null) {

      return Future.failedFuture("The state can not converted to JSON.");

    } else {

      this.storeState(object, promise);

    }

    return Model.fromFutureJsonObject(promise.future(), State.class);
  }

  /**
   * Store a state.
   *
   * @param state        to store.
   * @param storeHandler handler to manage the search.
   */
  void storeState(JsonObject state, Handler<AsyncResult<JsonObject>> storeHandler);

  /**
   * Update a state.
   *
   * @param state to update.
   *
   * @return the future update state.
   */
  @GenIgnore
  default Future<Void> updateState(final State state) {

    final Promise<Void> promise = Promise.promise();
    final var object = state.toJsonObject();
    if (object == null) {

      return Future.failedFuture("The state can not converted to JSON.");

    } else {

      this.updateState(object, promise);

    }
    return promise.future();
  }

  /**
   * Update a state.
   *
   * @param state         to update.
   * @param updateHandler handler to manage the update result.
   */
  void updateState(JsonObject state, Handler<AsyncResult<Void>> updateHandler);

}
