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
import eu.internetofus.common.vertx.QueryBuilder;
import eu.internetofus.common.vertx.Repository;
import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.mongo.MongoClient;

/**
 * Implementation of the {@link StatesRepository}.
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class StatesRepositoryImpl extends Repository implements StatesRepository {

  /**
   * The name of the collection that contains the states.
   */
  public static final String STATES_COLLECTION = "states";

  /**
   * Create a new service.
   *
   * @param pool    to create the connections.
   * @param version of the schemas.
   */
  public StatesRepositoryImpl(final MongoClient pool, final String version) {

    super(pool, version);

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void searchState(final String communityId, String taskId, String userId,
      final Handler<AsyncResult<JsonObject>> searchHandler) {

    final var query = new QueryBuilder().withNoExistNullEqOrRegex("communityId", communityId)
        .withNoExistNullEqOrRegex("taskId", taskId).withNoExistNullEqOrRegex("userId", userId).build();
    this.findOneDocument(STATES_COLLECTION, query, new JsonObject().put("_id", false), null).onComplete(searchHandler);

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void storeState(final JsonObject state, final Handler<AsyncResult<JsonObject>> storeHandler) {

    this.storeOneDocument(STATES_COLLECTION, state, stored -> {

      stored.remove("_id");
      return stored;

    }).onComplete(storeHandler);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void updateState(final JsonObject state, final Handler<AsyncResult<Void>> updateHandler) {

    final var query = new QueryBuilder().withNoExistNullEqOrRegex("communityId", state.getString("communityId"))
        .withNoExistNullEqOrRegex("taskId", state.getString("taskId"))
        .withNoExistNullEqOrRegex("userId", state.getString("userId")).build();
    this.updateOneDocument(STATES_COLLECTION, query, state).onComplete(updateHandler);
  }

  /**
   * Migrate the collections to the current version.
   *
   * @return the future that will inform if the migration is a success or not.
   */
  public Future<Void> migrateDocumentsToCurrentVersions() {

    return this.migrateCollection(STATES_COLLECTION, State.class);

  }
}
