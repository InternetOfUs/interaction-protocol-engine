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
import eu.internetofus.common.vertx.Repository;
import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Promise;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.mongo.FindOptions;
import io.vertx.ext.mongo.MongoClient;

/**
 * Implementation of the {@link InteractionsRepository}.
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class InteractionsRepositoryImpl extends Repository implements InteractionsRepository {

  /**
   * The name of the collection that contains the interactions.
   */
  public static final String INTERACTIONS_COLLECTION = "interactions";

  /**
   * Create a new service.
   *
   * @param vertx   event bus to use.
   * @param pool    to create the connections.
   * @param version of the schemas.
   */
  public InteractionsRepositoryImpl(final Vertx vertx, final MongoClient pool, final String version) {

    super(vertx, pool, version);

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void storeInteraction(final JsonObject interaction, final Handler<AsyncResult<JsonObject>> storeHandler) {

    this.storeOneDocument(INTERACTIONS_COLLECTION, interaction, stored -> {

      stored.remove("_id");
      return stored;

    }).onComplete(storeHandler);
  }

  /**
   * Migrate the collections to the current version.
   *
   * @return the future that will inform if the migration is a success or not.
   */
  public Future<Void> migrateDocumentsToCurrentVersions() {

    return this.migrateCollection(INTERACTIONS_COLLECTION, Interaction.class);

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void retrieveInteractionsPage(final JsonObject query, final JsonObject order, final int offset,
      final int limit, final Handler<AsyncResult<JsonObject>> handler) {

    final var options = new FindOptions();
    options.setSort(order);
    options.setSkip(offset);
    options.setLimit(limit);
    this.searchPageObject(INTERACTIONS_COLLECTION, query, options, "interactions",
        interaction -> interaction.remove("_id")).onComplete(handler);

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void deleteInteractions(final JsonObject query, final Handler<AsyncResult<Void>> handler) {

    this.pool.removeDocuments(INTERACTIONS_COLLECTION, query).compose(result -> {

      final Promise<Void> promise = Promise.promise();
      if (result.getRemovedCount() > 0) {

        promise.complete();

      } else {

        promise.fail("Any document removed");
      }

      return promise.future();
    }).onComplete(handler);
  }
}
